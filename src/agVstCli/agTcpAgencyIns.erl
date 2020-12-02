-module(agTcpAgencyIns).
-include("agVstCli.hrl").
-include("eArango.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   %% Inner Behavior API
   init/1
   , handleMsg/3
   , terminate/3
]).

-spec init(term()) -> no_return().
init({PoolName, AgencyName, #agencyOpts{reconnect = Reconnect, backlogSize = BacklogSize, reconnectTimeMin = Min, reconnectTimeMax = Max}}) ->
   ReconnectState = agAgencyUtils:initReConnState(Reconnect, Min, Max),
   self() ! ?AgMDoNetConn,
   {ok, #srvState{poolName = PoolName, serverName = AgencyName, reconnectState = ReconnectState}, #cliState{backlogSize = BacklogSize}}.

-spec handleMsg(term(), srvState(), cliState()) -> {ok, term(), term()}.
handleMsg(#agReq{method = Method, path = Path, queryPars = QueryPars, headers = Headers, body = Body, messageId = MessageId, fromPid = FromPid, overTime = OverTime, isSystem = IsSystem},
   #srvState{serverName = ServerName, dbName = DbName, socket = Socket} = SrvState,
   #cliState{backlogNum = BacklogNum, backlogSize = BacklogSize} = CliState) ->
   case Socket of
      undefined ->
         agAgencyUtils:agencyReply(FromPid, MessageId, undefined, {error, noSocket}),
         {ok, SrvState, CliState};
      _ ->
         case BacklogNum >= BacklogSize of
            true ->
               ?AgWarn(ServerName, ":backlog full curNum:~p Total: ~p ~n", [BacklogNum, BacklogSize]),
               agAgencyUtils:agencyReply(FromPid, MessageId, undefined, {error, backlogFull}),
               {ok, SrvState, CliState};
            _ ->
               Request = agVstProtocol:request(IsSystem, Method, DbName, Path, QueryPars, Headers, Body),
               case gen_tcp:send(Socket, Request) of
                  ok ->
                     TimerRef = case OverTime of
                        infinity ->
                           undefined;
                        _ ->
                           erlang:start_timer(OverTime, self(), {mWaitingOver, MessageId, FromPid}, [{abs, true}])
                     end,
                     erlang:put(MessageId, {FromPid, TimerRef, 0, <<>>}),
                     {ok, SrvState, CliState#cliState{backlogNum = BacklogNum + 1}};
                  {error, Reason} ->
                     ?AgWarn(ServerName, ":send error: ~p ~p ~p ~n", [Reason, FromPid, MessageId]),
                     gen_tcp:close(Socket),
                     agAgencyUtils:agencyReply(FromPid, MessageId, undefined, {error, {socketSendError, Reason}}),
                     agAgencyUtils:dealClose(SrvState, CliState, {error, {socketSendError, Reason}})
               end
         end
   end;
handleMsg({tcp, Socket, Data},
   #srvState{serverName = ServerName, socket = Socket} = SrvState,
   #cliState{revStatus = RevStatus, backlogNum = BacklogNum, messageId = MessageId, chunkIdx = ChunkIdx,  chunkSize = ChunkSize, chunkBuffer = ChunkBuffer} = CliState) ->
   case agVstProtocol:response(RevStatus, MessageId, ChunkIdx, ChunkSize, ChunkBuffer, Data) of
      ?AgCDone ->
         {ok, SrvState, CliState#cliState{revStatus = ?AgCUndef, chunkBuffer = <<>>}};
      {?AgMDone, MsgBuffer}  ->
         agAgencyUtils:agencyReply(MessageId, MsgBuffer),
         {ok, SrvState, CliState#cliState{revStatus = ?AgCUndef, backlogNum = BacklogNum - 1, chunkBuffer = <<>>}};
      {?AgCBodyStart, MessageId, ChunkIdx, ChunkSize, ChunkBuffer} ->
         {ok, SrvState, CliState#cliState{revStatus = ?AgCBody, messageId = MessageId, chunkIdx = ChunkIdx, chunkSize = ChunkSize, chunkBuffer = ChunkBuffer}};
      {?AgCBodyGoOn, ChunkBuffer} ->
         {ok, SrvState, CliState#cliState{revStatus = ?AgCBody, chunkBuffer = ChunkBuffer}};
      {?AgCHeader, ChunkBuffer} ->
         {ok, SrvState, CliState#cliState{revStatus = ?AgCHeader, chunkBuffer = ChunkBuffer}};
      {error, Err} ->
         ?AgWarn(ServerName, "handleMsg_tcp error happen ~p ~p ~p ~n", [Err, SrvState, CliState]),
         {ok, SrvState, CliState}
   end;
handleMsg({timeout, _TimerRef, {mWaitingOver, MessageId, FromPid}}, SrvState,
   #cliState{backlogNum = BacklogNum} = CliState) ->
   agAgencyUtils:agencyReply(FromPid, MessageId, undefined, {error, timeout}),
   {ok, SrvState, CliState#cliState{backlogNum = BacklogNum - 1}};
handleMsg({tcp_closed, Socket},
   #srvState{socket = Socket, serverName = ServerName} = SrvState,
   CliState) ->
   ?AgWarn(ServerName, "connection closed~n", []),
   gen_tcp:close(Socket),
   agAgencyUtils:dealClose(SrvState, CliState, {error, tcp_closed});
handleMsg({tcp_error, Socket, Reason},
   #srvState{socket = Socket, serverName = ServerName} = SrvState,
   CliState) ->
   ?AgWarn(ServerName, "connection error: ~p~n", [Reason]),
   gen_tcp:close(Socket),
   agAgencyUtils:dealClose(SrvState, CliState, {error, {tcp_error, Reason}});
handleMsg(?AgMDoNetConn,
   #srvState{poolName = PoolName, serverName = ServerName, reconnectState = ReconnectState} = SrvState,
   CliState) ->
   case ?agBeamPool:getv(PoolName) of
      #dbOpts{host = Host, port = Port, hostname = HostName, dbName = DbName, userPassword = UserPassword, socketOpts = SocketOpts} ->
         case gen_tcp:connect(HostName, Port, SocketOpts, ?AgDefConnTimeout) of
            {ok, Socket} ->
               %% IMY-todo 这里进行连接认证信息
               {ok, Socket};
            {error, Reason} ->
               ?AgWarn(ServerName, "connect error: ~p~n", [Reason]),
               agAgencyUtils:reConnTimer(SrvState, CliState)
         end;
      _Ret ->
         ?AgWarn(ServerName, "deal connect not found agBeamPool:getv(~p) ret ~p is error ~n", [PoolName, _Ret])
   end;
handleMsg(Msg, #srvState{serverName = ServerName} = SrvState, CliState) ->
   ?AgWarn(ServerName, "unknown msg: ~p~n", [Msg]),
   {ok, SrvState, CliState}.

-spec terminate(term(), srvState(), cliState()) -> ok.
terminate(_Reason,
   #srvState{socket = Socket} = SrvState,
   CliState) ->
   {ok, NewSrvState, NewCliState} = overAllWork(SrvState, CliState),
   gen_tcp:close(Socket),
   agAgencyUtils:dealClose(NewSrvState, NewCliState, {error, shutdown}),
   ok.

-spec overAllWork(srvState(), cliState()) -> {ok, srvState(), cliState()}.
overAllWork(SrvState, #cliState{revStatus = Status} = CliState) ->
   ok.
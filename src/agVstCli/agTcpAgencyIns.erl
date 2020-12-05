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
init({PoolName, AgencyName, #agencyOpts{reconnect = Reconnect, backlogSize = BacklogSize, reConnTimeMin = Min, reConnTimeMax = Max}}) ->
   self() ! ?AgMDoDBConn,
   ReConnState = agAgencyUtils:initReConnState(Reconnect, Min, Max),
   {ok, #srvState{poolName = PoolName, serverName = AgencyName, reConnState = ReConnState}, #cliState{backlogSize = BacklogSize}}.

-spec handleMsg(term(), srvState(), cliState()) -> {ok, term(), term()}.
handleMsg(#agReq{method = Method, path = Path, queryPars = QueryPars, headers = Headers, body = Body, messageId = MessageId, fromPid = FromPid, overTime = OverTime, isSystem = IsSystem},
   #srvState{serverName = ServerName, dbName = DbName, socket = Socket, vstSize = VstSize} = SrvState,
   #cliState{backlogNum = BacklogNum, backlogSize = BacklogSize} = CliState) ->
   case Socket of
      undefined ->
         agAgencyUtils:agencyReply(FromPid, undefined, MessageId, {error, noSocket}),
         {ok, SrvState, CliState};
      _ ->
         case BacklogNum >= BacklogSize of
            true ->
               ?AgWarn(ServerName, ":backlog full curNum:~p Total: ~p ~n", [BacklogNum, BacklogSize]),
               agAgencyUtils:agencyReply(FromPid, undefined, MessageId, {error, backlogFull}),
               {ok, SrvState, CliState};
            _ ->
               Request = agVstProto:request(IsSystem, MessageId, Method, DbName, Path, QueryPars, Headers, Body, VstSize),
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
                     agAgencyUtils:agencyReply(FromPid, undefined, MessageId, {error, {socketSendError, Reason}}),
                     agAgencyUtils:dealClose(SrvState, CliState, {error, {socketSendError, Reason}})
               end
         end
   end;
handleMsg({tcp, _Socket, Data}, SrvState,
   #cliState{revStatus = RevStatus, backlogNum = BacklogNum, messageId = MessageId, chunkIdx = ChunkIdx,  chunkSize = ChunkSize, chunkBuffer = ChunkBuffer} = CliState) ->
   case agVstProto:response(RevStatus, 0, MessageId, ChunkIdx, ChunkSize, ChunkBuffer, Data) of
      {?AgUndef, DoneCnt} ->
         {ok, SrvState, CliState#cliState{revStatus = ?AgUndef, backlogNum = BacklogNum - DoneCnt, chunkBuffer = <<>>}};
      {?AgCBodyStart, DoneCnt, MessageId, ChunkIdx, ChunkSize, ChunkBuffer} ->
         {ok, SrvState, CliState#cliState{revStatus = ?AgCBody, backlogNum = BacklogNum - DoneCnt, messageId = MessageId, chunkIdx = ChunkIdx, chunkSize = ChunkSize, chunkBuffer = ChunkBuffer}};
      {?AgCBodyGoOn, DoneCnt, ChunkBuffer} ->
         {ok, SrvState, CliState#cliState{revStatus = ?AgCBody, backlogNum = BacklogNum - DoneCnt, chunkBuffer = ChunkBuffer}};
      {?AgCHeader, DoneCnt, ChunkBuffer} ->
         {ok, SrvState, CliState#cliState{revStatus = ?AgCHeader, backlogNum = BacklogNum - DoneCnt, chunkBuffer = ChunkBuffer}}
   end;
handleMsg({timeout, _TimerRef, {mWaitingOver, MessageId, FromPid}}, SrvState,
   #cliState{backlogNum = BacklogNum} = CliState) ->
   MsgCache = erlang:get(MessageId),
   MsgPF = erlang:setelement(?AgPFIdx, MsgCache, timeOut),
   erlang:put(MessageId, MsgPF),
   agAgencyUtils:agencyReTimeout(FromPid, MessageId, {error, timeout}),
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
handleMsg(?AgMDoDBConn,
   #srvState{poolName = PoolName, serverName = ServerName, reConnState = _ReConnState} = SrvState,
   CliState) ->
   case ?agBeamPool:getv(PoolName) of
      #dbOpts{port = Port, hostname = HostName, dbName = DbName, user = User, password = Password, vstSize = VstSize} ->
         case gen_tcp:connect(HostName, Port, ?AgDefSocketOpts, ?AgDefConnTimeout) of
            {ok, Socket} ->
               gen_tcp:send(Socket, ?AgUpgradeInfo),
               AuthInfo = agVstProto:authInfo(User, Password),
               gen_tcp:send(Socket, AuthInfo),
               case agVstCli:receiveTcpData(#recvState{}, Socket) of
                  {ok, MsgBin} ->
                     case eVPack:decode(MsgBin) of
                        [1, 2, 200, _] ->
                           {ok, SrvState#srvState{dbName = DbName, socket = Socket, vstSize = VstSize}, CliState};
                        _Err ->
                           ?AgWarn(ServerName, "auth error: ~p~n", [_Err]),
                           agAgencyUtils:reConnTimer(SrvState, CliState)
                     end;
                  {error, Reason} = Err ->
                     ?AgWarn(ServerName, "recv auth error: ~p~n", [Reason]),
                     Err
               end;
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
terminate(_Reason, #srvState{socket = Socket} = SrvState, CliState) ->
   {ok, NewSrvState, NewCliState} = waitAllReqOver(SrvState, CliState),
   gen_tcp:close(Socket),
   agAgencyUtils:dealClose(NewSrvState, NewCliState, {error, shutdown}),
   ok.

-spec waitAllReqOver(srvState(), cliState()) -> {ok, srvState(), cliState()}.
waitAllReqOver(SrvState, #cliState{backlogNum = BacklogNum} = CliState) ->
   case BacklogNum > 0 of
      true ->
         receive
            Msg ->
               {ok, NewSrvState, NewCliState} = handleMsg(Msg, SrvState, CliState),
               waitAllReqOver(NewSrvState, NewCliState)
         end;
      _ ->
         {ok, SrvState, CliState}
   end.

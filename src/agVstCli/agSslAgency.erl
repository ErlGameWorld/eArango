-module(agSslAgency).

-include("agVstCli.hrl").
-include("eArango.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   start_link/3

   , init_it/3
   , loop/3
   , system_code_change/4
   , system_continue/3
   , system_get_state/1
   , system_terminate/4
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genActor  start %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link(module(), term(), [proc_lib:spawn_option()]) -> {ok, pid()}.
start_link(ServerName, Args, SpawnOpts) ->
   proc_lib:start_link(?MODULE, init_it, [ServerName, self(), Args], infinity, SpawnOpts).

init_it(ServerName, Parent, Args) ->
   case safeRegister(ServerName) of
      true ->
         process_flag(trap_exit, true),
         moduleInit(Parent, Args);
      {false, Pid} ->
         proc_lib:init_ack(Parent, {error, {alreadyStarted, Pid}})
   end.

-spec system_code_change(term(), module(), undefined | term(), term()) -> {ok, term()}.
system_code_change(MiscState, _Module, _OldVsn, _Extra) ->
   {ok, MiscState}.

-spec system_continue(pid(), [], {module(), term(), term()}) -> ok.
system_continue(_Parent, _Debug, {Parent, SrvState, CliState}) ->
   ?MODULE:loop(Parent, SrvState, CliState).

-spec system_get_state(term()) -> {ok, term()}.
system_get_state({_Parent, SrvState, _CliState}) ->
   {ok, SrvState}.

-spec system_terminate(term(), pid(), [], term()) -> none().
system_terminate(Reason, _ParentS, _Debug, {_Parent, SrvState, CliState}) ->
   terminate(Reason, SrvState, CliState).

safeRegister(ServerName) ->
   try register(ServerName, self()) of
      true -> true
   catch
      _:_ -> {false, whereis(ServerName)}
   end.

moduleInit(Parent, Args) ->
   case init(Args) of
      {ok, SrvState, CliState} ->
         proc_lib:init_ack(Parent, {ok, self()}),
         ?MODULE:loop(Parent, SrvState, CliState);
      {stop, Reason} ->
         proc_lib:init_ack(Parent, {error, Reason}),
         exit(Reason)
   end.

loop(Parent, SrvState, CliState) ->
   receive
      {system, From, Request} ->
         sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {Parent, SrvState, CliState});
      {'EXIT', Parent, Reason} ->
         terminate(Reason, SrvState, CliState);
      Msg ->
         {ok, NewSrvState, NewCliState} = handleMsg(Msg, SrvState, CliState),
         ?MODULE:loop(Parent, NewSrvState, NewCliState)
   end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genActor  end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
               case ssl:send(Socket, Request) of
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
                     ?AgErr(ServerName, ":send error: ~p ~p ~p ~n", [Reason, FromPid, MessageId]),
                     ssl:close(Socket),
                     agAgencyUtils:agencyReply(FromPid, undefined, MessageId, {error, {socketSendError, Reason}}),
                     agAgencyUtils:dealClose(SrvState, CliState, {error, {socketSendError, Reason}})
               end
         end
   end;
handleMsg({ssl, _Socket, DataBuffer}, SrvState,
   #cliState{revStatus = RevStatus, backlogNum = BacklogNum, messageId = OldMessageId, chunkIdx = OldChunkIdx,  chunkSize = OldChunkSize, chunkBuffer = OldChunkBuffer} = CliState) ->
   case agVstProto:response(RevStatus, 0, OldMessageId, OldChunkIdx, OldChunkSize, OldChunkBuffer, DataBuffer) of
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
handleMsg({ssl_closed, Socket},
   #srvState{socket = Socket, serverName = ServerName} = SrvState,
   CliState) ->
   ?AgInfo(ServerName, "connection closed~n", []),
   ssl:close(Socket),
   agAgencyUtils:dealClose(SrvState, CliState, {error, ssl_closed});
handleMsg({ssl_error, Socket, Reason},
   #srvState{socket = Socket, serverName = ServerName} = SrvState,
   CliState) ->
   ?AgInfo(ServerName, "connection error: ~p~n", [Reason]),
   ssl:close(Socket),
   agAgencyUtils:dealClose(SrvState, CliState, {error, {ssl_error, Reason}});
handleMsg(?AgMDoDBConn,
   #srvState{poolName = PoolName, serverName = ServerName, reConnState = ReConnState} = SrvState,
   CliState) ->
   case ?agBeamPool:getv(PoolName) of
      #dbOpts{port = Port, hostname = HostName, dbName = DbName, user = User, password = Password, vstSize = VstSize} ->
         case ssl:connect(HostName, Port, ?AgDefSocketOpts, ?AgDefConnTimeout) of
            {ok, Socket} ->
               ssl:send(Socket, ?AgUpgradeInfo),
               AuthInfo = agVstProto:authInfo(User, Password),
               ssl:send(Socket, AuthInfo),
               case agVstCli:receiveSslData(#recvState{}, Socket) of
                  {200, _BodyMap, _HeaderMap} ->
                     {ok, SrvState#srvState{dbName = DbName, reConnState = agAgencyUtils:resetReConnState(ReConnState), socket = Socket, vstSize = VstSize}, CliState};
                  _Err ->
                     ?AgErr(ServerName, "auth error: ~p~n", [_Err]),
                     agAgencyUtils:reConnTimer(SrvState, CliState)
               end;
            _Err ->
               ?AgErr(ServerName, "connect error: ~p~n", [_Err]),
               agAgencyUtils:reConnTimer(SrvState, CliState)
         end;
      _Ret ->
         ?AgErr(ServerName, "deal connect not found agBeamPool:getv(~p) ret ~p is error ~n", [PoolName, _Ret])
   end;
handleMsg({'$gen_call', FromTag, '$SrvInfo'}, SrvState, CliState) ->
   {To, Tag} = FromTag,
   catch To ! {Tag, {erlang:get(), SrvState, CliState}},
   {ok, SrvState, CliState};
handleMsg(Msg, #srvState{serverName = ServerName} = SrvState, CliState) ->
   ?AgErr(ServerName, "unknown msg: ~p~n", [Msg]),
   {ok, SrvState, CliState}.

-spec terminate(term(), srvState(), cliState()) -> ok.
terminate(Reason, #srvState{socket = Socket} = SrvState, CliState) ->
   {ok, NewSrvState, NewCliState} = waitAllReqOver(SrvState, CliState),
   case Socket of
      undefined ->
         ignore;
      _ ->
         ssl:close(Socket)
   end,
   agAgencyUtils:dealClose(NewSrvState, NewCliState, {error, shutdown}),
   exit(Reason).

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

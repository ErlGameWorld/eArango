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
   ReconnectState = agAgencyUtils:initReconnectState(Reconnect, Min, Max),
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
                     erlang:put(MessageId, {FromPid, TimerRef, 0, <<>>, 0, 0, <<>>}),
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
   #cliState{revStatus = RevStatus, backlogNum = BacklogNum, buffer = Buffer} = CliState) ->
   try agVstProtocol:response(RevStatus, Buffer, Data) of
      {done, #recvState{statusCode = StatusCode, headers = Headers, body = Body}} ->
         agAgencyUtils:agencyReply(CurInfo, {StatusCode, Body, Headers}),
         case RequestsOuts of
            [] ->
               case RequestsIns of
                  [] ->
                     {ok, SrvState, CliState#cliState{backlogNum = BacklogNum - 1, revStatus = leisure, curInfo = undefined, recvState = undefined}};
                  [MiRequest] ->
                     dealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsIns = [], backlogNum = BacklogNum - 1, revStatus = leisure, curInfo = undefined, recvState = undefined});
                  MiRLists ->
                     [MiRequest | Outs] = lists:reverse(MiRLists),
                     dealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsIns = [], requestsOuts = Outs, backlogNum = BacklogNum - 1, revStatus = leisure, curInfo = undefined, recvState = undefined})
               end;
            [MiRequest] ->
               dealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsOuts = [], backlogNum = BacklogNum - 1, revStatus = leisure, curInfo = undefined, recvState = undefined});
            [MiRequest | Outs] ->
               dealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsOuts = Outs, backlogNum = BacklogNum - 1, revStatus = leisure, curInfo = undefined, recvState = undefined})
         end;
      {ok, NewRecvState} ->
         {ok, SrvState, CliState#cliState{recvState = NewRecvState}};
      {error, Reason} ->
         ?AgWarn(ServerName, "handle tcp data error: ~p ~p ~n", [Reason, CurInfo]),
         gen_tcp:close(Socket),
         agAgencyUtils:dealClose(SrvState, CliState, {error, {tcpDataError, Reason}})
   catch
      E:R:S ->
         ?AgWarn(ServerName, "handle tcp data crash: ~p:~p~n~p~n ~p ~n ", [E, R, S, CurInfo]),
         gen_tcp:close(Socket),
         agAgencyUtils:dealClose(SrvState, CliState, {error, agencyHandledataError})
   end;
handleMsg({timeout, _TimerRef, {mWaitingOver, MessageId, FromPid}},
   SrvState,
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
         case dealConnect(ServerName, HostName, Port, SocketOpts) of
            {ok, Socket} ->
               NewReconnectState = agAgencyUtils:resetReconnectState(ReconnectState),
               %% 新建连接之后 需要重置之前的buff之类状态数据
               case RequestsOuts of
                  [] ->
                     case RequestsIns of
                        [] ->
                           {ok, SrvState#srvState{userPassWord = UserPassword, dbName = DbName, host = Host, reconnectState = NewReconnectState, socket = Socket}, CliState#cliState{revStatus = leisure, curInfo = undefined, recvState = undefined}};
                        [MiRequest] ->
                           dealQueueRequest(MiRequest, SrvState#srvState{socket = Socket, reconnectState = NewReconnectState}, CliState#cliState{requestsIns = [], revStatus = leisure, curInfo = undefined, recvState = undefined});
                        MiRLists ->
                           [MiRequest | Outs] = lists:reverse(MiRLists),
                           dealQueueRequest(MiRequest, SrvState#srvState{socket = Socket, reconnectState = NewReconnectState}, CliState#cliState{requestsIns = [], requestsOuts = Outs, revStatus = leisure, curInfo = undefined, recvState = undefined})
                     end;
                  [MiRequest] ->
                     dealQueueRequest(MiRequest, SrvState#srvState{socket = Socket, reconnectState = NewReconnectState}, CliState#cliState{requestsOuts = [], revStatus = leisure, curInfo = undefined, recvState = undefined});
                  [MiRequest | Outs] ->
                     dealQueueRequest(MiRequest, SrvState#srvState{socket = Socket, reconnectState = NewReconnectState}, CliState#cliState{requestsOuts = Outs, revStatus = leisure, curInfo = undefined, recvState = undefined})
               end;
            {error, _Reason} ->
               agAgencyUtils:reconnectTimer(SrvState, CliState)
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
overAllWork(SrvState, #cliState{requestsIns = RequestsIns, requestsOuts = RequestsOuts, revStatus = Status} = CliState) ->
   case Status of
      leisure ->
         case RequestsOuts of
            [] ->
               case RequestsIns of
                  [] ->
                     {ok, SrvState, CliState};
                  [MiRequest] ->
                     dealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsIns = []});
                  MiRLists ->
                     [MiRequest | Outs] = lists:reverse(MiRLists),
                     dealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsIns = [], requestsOuts = Outs})
               end;
            [MiRequest] ->
               dealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsOuts = []});
            [MiRequest | Outs] ->
               dealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsOuts = Outs})
         end;
      _ ->
         overReceiveTcpData(SrvState, CliState)
   end.

-spec overDealQueueRequest(miRequest(), srvState(), cliState()) -> {ok, srvState(), cliState()}.
overDealQueueRequest(#agReq{method = Method, path = Path, headers = Headers, body = Body, messageId = RequestId, fromPid = FromPid, overTime = OverTime, isSystem = IsSystem},
   #srvState{serverName = ServerName, host = Host, userPassWord = UserPassWord, dbName = DbName, socket = Socket} = SrvState,
   #cliState{requestsIns = RequestsIns, requestsOuts = RequestsOuts, backlogNum = BacklogNum} = CliState) ->
   case erlang:monotonic_time(millisecond) > OverTime of
      true ->
         %% 超时了
         agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, timeout}),
         case RequestsOuts of
            [] ->
               case RequestsIns of
                  [] ->
                     {ok, SrvState, CliState#cliState{backlogNum = BacklogNum - 1}};
                  [MiRequest] ->
                     overDealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsIns = [], backlogNum = BacklogNum - 1});
                  MiRLists ->
                     [MiRequest | Outs] = lists:reverse(MiRLists),
                     overDealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsIns = [], requestsOuts = Outs, backlogNum = BacklogNum - 1})
               end;
            [MiRequest] ->
               overDealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsOuts = [], backlogNum = BacklogNum - 1});
            [MiRequest | Outs] ->
               overDealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsOuts = Outs, backlogNum = BacklogNum - 1})
         end;
      _ ->
         Request = agVstProtocol:request(IsSystem, Body, Method, Host, DbName, Path, [UserPassWord | Headers]),
         case gen_tcp:send(Socket, Request) of
            ok ->
               TimerRef =
                  case OverTime of
                     infinity ->
                        undefined;
                     _ ->
                        erlang:start_timer(OverTime, self(), mWaitingOver, [{abs, true}])
                  end,
               overReceiveTcpData(SrvState, CliState#cliState{isHeadMethod = Method == ?AgHead, revStatus = waiting, curInfo = {FromPid, RequestId, TimerRef}});
            {error, Reason} ->
               ?AgWarn(ServerName, ":send error: ~p~n", [Reason]),
               gen_tcp:close(Socket),
               agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, socketSendError}),
               agAgencyUtils:dealClose(SrvState, CliState, {error, socketSendError})
         end
   end.

-spec overReceiveTcpData(srvState(), cliState()) -> {ok, srvState(), cliState()}.
overReceiveTcpData(#srvState{poolName = PoolName, serverName = ServerName, rn = Rn, rnrn = RnRn, socket = Socket} = SrvState,
   #cliState{isHeadMethod = IsHeadMethod, backlogNum = BacklogNum, curInfo = CurInfo, requestsIns = RequestsIns, requestsOuts = RequestsOuts, recvState = RecvState} = CliState) ->
   receive
      {tcp, Socket, Data} ->
         try agVstProtocol:response(RecvState, Rn, RnRn, Data, IsHeadMethod) of
            {done, #recvState{statusCode = StatusCode, headers = Headers, body = Body}} ->
               agAgencyUtils:agencyReply(CurInfo, {StatusCode, Body, Headers}),
               case RequestsOuts of
                  [] ->
                     case RequestsIns of
                        [] ->
                           {ok, SrvState, CliState#cliState{backlogNum = BacklogNum - 1, revStatus = leisure, curInfo = undefined, recvState = undefined}};
                        [MiRequest] ->
                           dealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsIns = [], backlogNum = BacklogNum - 1, revStatus = leisure, curInfo = undefined, recvState = undefined});
                        MiRLists ->
                           [MiRequest | Outs] = lists:reverse(MiRLists),
                           dealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsIns = [], requestsOuts = Outs, backlogNum = BacklogNum - 1, revStatus = leisure, curInfo = undefined, recvState = undefined})
                     end;
                  [MiRequest] ->
                     dealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsOuts = [], backlogNum = BacklogNum - 1, revStatus = leisure, curInfo = undefined, recvState = undefined});
                  [MiRequest | Outs] ->
                     dealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsOuts = Outs, backlogNum = BacklogNum - 1, revStatus = leisure, curInfo = undefined, recvState = undefined})
               end;
            {ok, NewRecvState} ->
               overReceiveTcpData(SrvState, CliState#cliState{recvState = NewRecvState});
            {error, Reason} ->
               ?AgWarn(overReceiveTcpData, "handle tcp data error: ~p ~n", [Reason]),
               gen_tcp:close(Socket),
               agAgencyUtils:dealClose(SrvState, CliState, {error, {tcpDataError, Reason}})
         catch
            E:R:S ->
               ?AgWarn(overReceiveTcpData, "handle tcp data crash: ~p:~p~n~p ~n ", [E, R, S]),
               gen_tcp:close(Socket),
               agAgencyUtils:dealClose(SrvState, CliState, {error, {tcp_error, handledataError}})
         end;
      {timeout, TimerRef, mWaitingOver} ->
         case CurInfo of
            {_PidForm, _RequestId, TimerRef} ->
               gen_tcp:close(Socket),
               agAgencyUtils:agencyReply(CurInfo, {error, timeout}),

               case RequestsOuts of
                  [] ->
                     case RequestsIns of
                        [] ->
                           {ok, SrvState, CliState#cliState{backlogNum = BacklogNum - 1, revStatus = leisure, curInfo = undefined, recvState = undefined}};
                        [MiRequest] ->
                           case ?agBeamPool:getv(PoolName) of
                              #dbOpts{port = Port, hostname = HostName, socketOpts = SocketOpts} ->
                                 case dealConnect(ServerName, HostName, Port, SocketOpts) of
                                    {ok, NewSocket} ->
                                       overDealQueueRequest(MiRequest, SrvState#srvState{socket = NewSocket}, CliState#cliState{requestsIns = [], revStatus = leisure, curInfo = undefined, recvState = undefined});
                                    {error, _Reason} ->
                                       agAgencyUtils:dealClose(SrvState, CliState, {error, {newTcpConnectErrorOver, _Reason}})
                                 end;
                              _Ret ->
                                 agAgencyUtils:dealClose(SrvState, CliState, {error, {notFoundPoolName, PoolName}})
                           end;
                        MiRLists ->
                           [MiRequest | Outs] = lists:reverse(MiRLists),
                           case ?agBeamPool:getv(PoolName) of
                              #dbOpts{port = Port, hostname = HostName, socketOpts = SocketOpts} ->
                                 case dealConnect(ServerName, HostName, Port, SocketOpts) of
                                    {ok, NewSocket} ->
                                       overDealQueueRequest(MiRequest, SrvState#srvState{socket = NewSocket}, CliState#cliState{requestsIns = [], requestsOuts = Outs, revStatus = leisure, curInfo = undefined, recvState = undefined});
                                    {error, _Reason} ->
                                       agAgencyUtils:dealClose(SrvState, CliState, {error, {newTcpConnectErrorOver, _Reason}})
                                 end;
                              _Ret ->
                                 agAgencyUtils:dealClose(SrvState, CliState, {error, {notFoundPoolName, PoolName}})
                           end
                     end;
                  [MiRequest] ->
                     case ?agBeamPool:getv(PoolName) of
                        #dbOpts{port = Port, hostname = HostName, socketOpts = SocketOpts} ->
                           case dealConnect(ServerName, HostName, Port, SocketOpts) of
                              {ok, NewSocket} ->
                                 overDealQueueRequest(MiRequest, SrvState#srvState{socket = NewSocket}, CliState#cliState{requestsOuts = [], revStatus = leisure, curInfo = undefined, recvState = undefined});
                              {error, _Reason} ->
                                 agAgencyUtils:dealClose(SrvState, CliState, {error, {newTcpConnectErrorOver, _Reason}})
                           end;
                        _Ret ->
                           agAgencyUtils:dealClose(SrvState, CliState, {error, {notFoundPoolName, PoolName}})
                     end;
                  [MiRequest | Outs] ->
                     case ?agBeamPool:getv(PoolName) of
                        #dbOpts{port = Port, hostname = HostName, socketOpts = SocketOpts} ->
                           case dealConnect(ServerName, HostName, Port, SocketOpts) of
                              {ok, NewSocket} ->
                                 overDealQueueRequest(MiRequest, SrvState#srvState{socket = NewSocket}, CliState#cliState{requestsOuts = Outs, revStatus = leisure, curInfo = undefined, recvState = undefined});
                              {error, _Reason} ->
                                 agAgencyUtils:dealClose(SrvState, CliState, {error, {newTcpConnectErrorOver, _Reason}})
                           end;
                        _Ret ->
                           agAgencyUtils:dealClose(SrvState, CliState, {error, {notFoundPoolName, PoolName}})
                     end
               end;
            _ ->
               ?AgWarn(overReceiveTcpData, "receive waiting_over TimerRef not match: ~p~n", [TimerRef]),
               overReceiveTcpData(SrvState, CliState)
         end;
      {tcp_closed, Socket} ->
         gen_tcp:close(Socket),
         agAgencyUtils:dealClose(SrvState, CliState, {error, tcp_closed});
      {tcp_error, Socket, Reason} ->
         gen_tcp:close(Socket),
         agAgencyUtils:dealClose(SrvState, CliState, {error, {tcp_error, Reason}});
      #agReq{} = MiRequest ->
         overReceiveTcpData(SrvState, CliState#cliState{requestsIns = [MiRequest | RequestsIns], backlogNum = BacklogNum + 1});
      _Msg ->
         ?AgWarn(overReceiveTcpData, "receive unexpect msg: ~p~n", [_Msg]),
         overReceiveTcpData(SrvState, CliState)
   end.

-spec dealConnect(atom(), hostName(), port(), socketOpts()) -> {ok, socket()} | {error, term()}.
dealConnect(ServerName, HostName, Port, SocketOptions) ->
   case inet:getaddrs(HostName, inet) of
      {ok, IPList} ->
         Ip = agMiscUtils:randomElement(IPList),
         case gen_tcp:connect(Ip, Port, SocketOptions, ?AgDefConnTimeout) of
            {ok, Socket} ->
               {ok, Socket};
            {error, Reason} ->
               ?AgWarn(ServerName, "connect error: ~p~n", [Reason]),
               {error, Reason}
         end;
      {error, Reason} ->
         ?AgWarn(ServerName, "getaddrs error: ~p~n", [Reason]),
         {error, Reason}
   end.

-spec dealQueueRequest(miRequest(), srvState(), cliState()) -> {ok, srvState(), cliState()}.
dealQueueRequest(#agReq{method = Method, path = Path, headers = Headers, body = Body, messageId = RequestId, fromPid = FromPid, overTime = OverTime, isSystem = IsSystem},
   #srvState{serverName = ServerName, host = Host, userPassWord = UserPassWord, dbName = DbName, socket = Socket} = SrvState,
   #cliState{requestsIns = RequestsIns, requestsOuts = RequestsOuts, backlogNum = BacklogNum} = CliState) ->
   case erlang:monotonic_time(millisecond) > OverTime of
      true ->
         %% 超时了
         agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, timeout}),
         case RequestsOuts of
            [] ->
               case RequestsIns of
                  [] ->
                     {ok, SrvState, CliState#cliState{backlogNum = BacklogNum - 1}};
                  [MiRequest] ->
                     dealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsIns = [], backlogNum = BacklogNum - 1});
                  MiRLists ->
                     [MiRequest | Outs] = lists:reverse(MiRLists),
                     dealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsIns = [], requestsOuts = Outs, backlogNum = BacklogNum - 1})
               end;
            [MiRequest] ->
               dealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsOuts = [], backlogNum = BacklogNum - 1});
            [MiRequest | Outs] ->
               dealQueueRequest(MiRequest, SrvState, CliState#cliState{requestsOuts = Outs, backlogNum = BacklogNum - 1})
         end;
      _ ->
         Request = agVstProtocol:request(IsSystem, Body, Method, Host, DbName, Path, [UserPassWord | Headers]),
         case gen_tcp:send(Socket, Request) of
            ok ->
               TimerRef =
                  case OverTime of
                     infinity ->
                        undefined;
                     _ ->
                        erlang:start_timer(OverTime, self(), mWaitingOver, [{abs, true}])
                  end,
               {ok, SrvState, CliState#cliState{isHeadMethod = Method == ?AgHead, revStatus = waiting, curInfo = {FromPid, RequestId, TimerRef}}};
            {error, Reason} ->
               ?AgWarn(ServerName, ":send error: ~p~n", [Reason]),
               gen_tcp:close(Socket),
               agAgencyUtils:agencyReply(FromPid, RequestId, undefined, {error, socketSendError}),
               agAgencyUtils:dealClose(SrvState, CliState, {error, socketSendError})
         end
   end.


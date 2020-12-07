-module(agVstCli).
-include("agVstCli.hrl").
-include("eArango.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   %% Common Request API
   callAgency/3
   , callAgency/6
   , callAgency/7
   , callAgency/8
   , castAgency/6
   , castAgency/7
   , castAgency/8
   , castAgency/9
   , receiveReqRet/2

   %% Pools API
   , startPool/2
   , startPool/3
   , stopPool/1

   %% Single Process DbAPI
   , connDb/1
   , disConnDb/1
   , getCurDbInfo/1
   , useDatabase/2

   , initMsgId/0
   , getMsgId/0
   , receiveTcpData/2
   , receiveSslData/2
]).


-spec callAgency(poolNameOrSocket(), method(), path()) -> term() | {error, term()}.
callAgency(PoolNameOrSocket, Method, Path) ->
   callAgency(PoolNameOrSocket, Method, Path, #{}, #{}, <<>>, false, ?AgDefTimeout).

-spec callAgency(poolNameOrSocket(), method(), path(), queryPars(), headers(), body()) -> term() | {error, term()}.
callAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body) ->
   callAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, false, ?AgDefTimeout).

-spec callAgency(poolNameOrSocket(), method(), path(), queryPars(), headers(), body(), boolean()) -> term() | {error, atom()}.
callAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, IsSystem) ->
   callAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, IsSystem, ?AgDefTimeout).

-spec callAgency(poolNameOrSocket(), method(), path(), queryPars(), headers(), body(), boolean(), timeout()) -> term() | {error, atom()}.
callAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, IsSystem, Timeout) ->
   case castAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, eVPack:encodeBin(Body), self(), IsSystem, Timeout) of
      {waitRRT, RequestId, MonitorRef} ->
         receiveReqRet(RequestId, MonitorRef);
      {error, _Reason} = Err ->
         Err;
      Ret ->
         Ret
   end.

-spec castAgency(poolNameOrSocket(), method(), path(), queryPars(), headers(), body()) -> {ok, messageId()} | {error, atom()}.
castAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body) ->
   castAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, self(), false, ?AgDefTimeout).

-spec castAgency(poolNameOrSocket(), method(), path(), queryPars(), headers(), body(), boolean()) -> {ok, messageId()} | {error, atom()}.
castAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, IsSystem) ->
   castAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, self(), IsSystem, ?AgDefTimeout).

-spec castAgency(poolNameOrSocket(), method(), path(), queryPars(), headers(), body(), boolean(), timeout()) -> {ok, messageId()} | {error, atom()}.
castAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, IsSystem, Timeout) ->
   castAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, self(), IsSystem, Timeout).

-spec castAgency(poolNameOrSocket(), method(), path(), queryPars(), headers(), body(), pid(), boolean(), timeout()) -> {ok, messageId()} | {error, atom()}.
castAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, Pid, IsSystem, Timeout) ->
   OverTime =
      case Timeout of
         infinity -> infinity;
         _ ->
            erlang:monotonic_time(millisecond) + Timeout
      end,
   case erlang:is_atom(PoolNameOrSocket) of
      true ->
         case agAgencyPoolMgrIns:getOneAgency(PoolNameOrSocket) of
            {error, pool_not_found} = Err ->
               Err;
            undefined ->
               {error, undefined_server};
            AgencyName ->
               MonitorRef = erlang:monitor(process, AgencyName),
               MessageId = getMsgId(),
               catch AgencyName ! #agReq{method = Method, path = Path, queryPars = QueryPars, headers = Headers, body = Body, messageId = MessageId, fromPid = Pid, overTime = OverTime, isSystem = IsSystem},
               {waitRRT, MessageId, MonitorRef}
         end;
      _ ->
         case getCurDbInfo(PoolNameOrSocket) of
            {DbName, VstSize, Protocol} ->
               Request = agVstProto:request(IsSystem, Method, DbName, Path, QueryPars, Headers, Body, VstSize),
               case Protocol of
                  tcp ->
                     case gen_tcp:send(PoolNameOrSocket, Request) of
                        ok ->
                           receiveTcpData(#recvState{}, PoolNameOrSocket);
                        {error, Reason} = Err ->
                           ?AgWarn(castAgency, ":gen_tcp send error: ~p ~n", [Reason]),
                           disConnDb(PoolNameOrSocket),
                           Err
                     end;
                  ssl ->
                     case ssl:send(PoolNameOrSocket, Request) of
                        ok ->
                           receiveSslData(#recvState{}, PoolNameOrSocket);
                        {error, Reason} = Err ->
                           ?AgWarn(castAgency, ":ssl send error: ~p ~n", [Reason]),
                           disConnDb(PoolNameOrSocket),
                           Err
                     end
               end;
            _ ->
               {error, dbinfoNotFound}
         end
   end.

-spec receiveReqRet(messageId(), reference()) -> {StatusCode :: non_neg_integer(), Body :: map(), Headers :: map()} | {error, term()}.
receiveReqRet(RequestId, MonitorRef) ->
   receive
      #agReqRet{messageId = RequestId, reply = Reply} ->
         erlang:demonitor(MonitorRef),
         case Reply of
            {error, Err} ->
               Err;
            _ ->
               ?AgWarn(tt, "IMY*************~p~n", [Reply]),
               {[1, 2, StatusCode, HeaderMap], BodyMap} = eVPack:decodeAll(Reply),
               {StatusCode, BodyMap, HeaderMap}
         end;
      {'DOWN', MonitorRef, process, _Pid, Reason} ->
         {error, {agencyDown, Reason}}
   end.

-spec receiveTcpData(recvState(), socket()) -> {ok, term(), term()} | {error, term()}.
receiveTcpData(RecvState, Socket) ->
   receive
      {tcp, Socket, DataBuffer} ->
         %% ?AgWarn(1111, "IMY************receove 1: ~p ~p ~n", [erlang:byte_size(DataBuffer), DataBuffer]),
         case agVstProto:response(element(2, RecvState), RecvState, DataBuffer) of
            {?AgMDone, MsgBin} ->
               {ok, MsgBin};
            {?AgCHeader, NewRecvState} ->
               receiveTcpData(NewRecvState, Socket);
            {?AgCBodyStart, NewRecvState} ->
               receiveTcpData(NewRecvState, Socket);
            {?AgCBodyGoOn, NewRecvState} ->
               receiveTcpData(NewRecvState, Socket)
         end;
      {tcp_closed, Socket} ->
         disConnDb(Socket),
         {error, tcp_closed};
      {tcp_error, Socket, Reason} ->
         disConnDb(Socket),
         {error, {tcp_error, Reason}}
   end.

-spec receiveSslData(recvState(), socket()) -> {ok, term(), term()} | {error, term()}.
receiveSslData(RecvState, Socket) ->
   receive
      {ssl, Socket, DataBuffer} ->
         case agVstProto:response(element(2, RecvState), RecvState, DataBuffer) of
            {?AgMDone, MsgBin} ->
               {ok, MsgBin};
            {?AgCHeader, NewRecvState} ->
               receiveTcpData(NewRecvState, Socket);
            {?AgCBodyStart, NewRecvState} ->
               receiveTcpData(NewRecvState, Socket);
            {?AgCBodyGoOn, NewRecvState} ->
               receiveTcpData(NewRecvState, Socket)
         end;
      {ssl_closed, Socket} ->
         disConnDb(Socket),
         {error, ssl_closed};
      {ssl_error, Socket, Reason} ->
         disConnDb(Socket),
         {error, {ssl_error, Reason}}
   end.

-spec startPool(poolName(), dbCfgs()) -> ok | {error, poolNameUsed}.
startPool(PoolName, DbCfgs) ->
   agAgencyPoolMgrIns:startPool(PoolName, DbCfgs, []).

-spec startPool(poolName(), dbCfgs(), agencyCfgs()) -> ok | {error, poolNameUsed}.
startPool(PoolName, DbCfgs, AgencyCfgs) ->
   agAgencyPoolMgrIns:startPool(PoolName, DbCfgs, AgencyCfgs).

-spec stopPool(poolName()) -> ok | {error, poolNotStarted}.
stopPool(PoolName) ->
   agAgencyPoolMgrIns:stopPool(PoolName).

-spec connDb(dbCfgs()) -> {ok, socket()} | {error, term()}.
connDb(DbCfgs) ->
   #dbOpts{
      port = Port,
      hostname = HostName,
      dbName = DbName,
      protocol = Protocol,
      user = User,
      password = Password,
      vstSize = VstSize
   } = agMiscUtils:dbOpts(DbCfgs),
   case Protocol of
      tcp ->
         case gen_tcp:connect(HostName, Port, ?AgDefSocketOpts, ?AgDefConnTimeout) of
            {ok, Socket} ->
               gen_tcp:send(Socket, ?AgUpgradeInfo),
               AuthInfo = agVstProto:authInfo(User, Password),
               gen_tcp:send(Socket, AuthInfo),
               case agVstCli:receiveTcpData(#recvState{}, Socket) of
                  {ok, MsgBin} ->
                     case eVPack:decodeHeader(MsgBin) of
                        [1, 2, 200, _] ->
                           setCurDbInfo(Socket, DbName, VstSize, Protocol),
                           {ok, Socket};
                        _Err ->
                           ?AgWarn(connDb_tcp, "auth error: ~p~n", [_Err]),
                           {error, _Err}
                     end;
                  {error, Reason} = Err ->
                     ?AgWarn(connDb_tcp, "recv error: ~p~n", [Reason]),
                     Err
               end;
            {error, Reason} = Err ->
               ?AgWarn(connDb_tcp, "connect error: ~p~n", [Reason]),
               Err
         end;
      ssl ->
         case ssl:connect(HostName, Port, ?AgDefSocketOpts, ?AgDefConnTimeout) of
            {ok, Socket} ->
               ssl:send(Socket, ?AgUpgradeInfo),
               AuthInfo = agVstProto:authInfo(User, Password),
               ssl:send(Socket, AuthInfo),
               case agVstCli:receiveSslData(#recvState{}, Socket) of
                  {ok, MsgBin} ->
                     case eVPack:decodeHeader(MsgBin) of
                        [1, 2, 200, _] ->
                           setCurDbInfo(Socket, DbName, VstSize, Protocol),
                           {ok, Socket};
                        _Err ->
                           ?AgWarn(connDb_ssl, "auth error: ~p~n", [_Err]),
                           {error, _Err}
                     end;
                  {error, Reason} = Err ->
                     ?AgWarn(connDb_ssl, "recv error: ~p~n", [Reason]),
                     Err
               end;
            {error, Reason} = Err ->
               ?AgWarn(connDb_ssl, "connect error: ~p~n", [Reason]),
               Err
         end
   end.

-spec disConnDb(socket()) -> ok | {error, term()}.
disConnDb(Socket) ->
   case erlang:erase({'$agDbInfo', Socket}) of
      undefined ->
         ignore;
      {_DbName, _VstSize, Protocol} ->
         case Protocol of
            tcp ->
               gen_tcp:close(Socket);
            ssl ->
               ssl:close(Socket)
         end
   end.

-spec setCurDbInfo(socket(), binary(), pos_integer(), protocol()) -> term().
setCurDbInfo(Socket, DbName, VstSize, Protocol) ->
   erlang:put({'$agDbInfo', Socket}, {DbName, VstSize, Protocol}).

-spec getCurDbInfo(socket()) -> term().
getCurDbInfo(Socket) ->
   erlang:get({'$agDbInfo', Socket}).

-spec useDatabase(socket(), binary()) -> ok.
useDatabase(Socket, NewDbName) ->
   case erlang:get({'$agDbInfo', Socket}) of
      undefined ->
         ignore;
      {_DbName, VstSize, Protocol} ->
         erlang:put({'$agDbInfo', Socket}, {NewDbName, VstSize, Protocol})
   end,
   ok.

initMsgId() ->
   case persistent_term:get(agMessageId, undefined) of
      undefined ->
         Ref = atomics:new(1, [{signed, false}]),
         InitId = rand:uniform(10000),
         atomics:put(Ref, 1, InitId),
         persistent_term:put(agMessageId, Ref);
      _ ->
         ignore
   end.

getMsgId() ->
   Ref = persistent_term:get(agMessageId, undefined),
   MessageId = atomics:add_get(Ref, 1, 1),
   if
      MessageId >= ?agMaxMessageId ->
         InitId = rand:uniform(10000),
         atomics:put(Ref, 1, InitId),
         InitId;
      true ->
         MessageId
   end.

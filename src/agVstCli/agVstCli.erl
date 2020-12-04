-module(agVstCli).
-include("agVstCli.hrl").
-include("eArango.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   %% Common Request API
   callAgency/6
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

   ,receiveTcpData/2
   ,receiveSslData/2
]).

-spec callAgency(poolNameOrSocket(), method(), path(), queryPars(), headers(), body()) -> term() | {error, term()}.
callAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body) ->
   callAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, false, ?AgDefTimeout).

-spec callAgency(poolNameOrSocket(), method(), path(), queryPars(), headers(), body(), boolean()) -> term() | {error, atom()}.
callAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, IsSystem) ->
   callAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, IsSystem, ?AgDefTimeout).

-spec callAgency(poolNameOrSocket(), method(), path(), queryPars(), headers(), body(), boolean(), timeout()) -> term() | {error, atom()}.
callAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, IsSystem, Timeout) ->
   case castAgency(PoolNameOrSocket, Method, Path, QueryPars, Headers, Body, self(), IsSystem, Timeout) of
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
               RequestId = {AgencyName, MonitorRef},
               catch AgencyName ! #agReq{method = Method, path = Path, queryPars = QueryPars, headers = Headers, body = Body, messageId = RequestId, fromPid = Pid, overTime = OverTime, isSystem = IsSystem},
               {waitRRT, RequestId, MonitorRef}
         end;
      _ ->
         case getCurDbInfo(PoolNameOrSocket) of
            {DbName, _UserPassWord, _Host, Protocol} ->
               Request = agVstProtoPl:request(IsSystem, Method, DbName, Path, QueryPars, Headers, Body),
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

-spec receiveReqRet(messageId(), reference()) -> {StatusCode :: non_neg_integer(), Body :: binary(), Headers :: binary()} | {error, term()}.
receiveReqRet(RequestId, MonitorRef) ->
   receive
      #agReqRet{messageId = RequestId, reply = Reply} ->
         erlang:demonitor(MonitorRef),
         case Reply of
            {_StatusCode, Body, _Headers} ->
               case Body of
                  <<>> ->
                     erlang:setelement(2, Reply, #{});
                  _ ->
                     erlang:setelement(2, Reply, jiffy:decode(Body, [return_maps, copy_strings]))
               end;
            _ ->
               Reply
         end;
      {'DOWN', MonitorRef, process, _Pid, Reason} ->
         {error, {agencyDown, Reason}}
   end.

-spec receiveTcpData(recvState() | undefined, socket()) -> {ok, term(), term()} | {error, term()}.
receiveTcpData(RecvState, Socket) ->
   receive
      {tcp, Socket, DataBuffer} ->
         ?AgWarn(1111, "receove : ~p~n", [DataBuffer]),
         case agVstProtoSp:response(element(1, RecvState), RecvState, DataBuffer) of
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

-spec receiveSslData(recvState() | undefined, socket()) -> {ok, term(), term()} | {error, term()}.
receiveSslData(RecvState, Socket) ->
   receive
      {ssl, Socket, DataBuffer} ->
         case agVstProtoSp:response(element(1, RecvState), RecvState, DataBuffer) of
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
      socketOpts = SocketOpts
   } = agMiscUtils:dbOpts(DbCfgs),
         case Protocol of
            tcp ->
               case gen_tcp:connect(HostName, Port, SocketOpts, ?AgDefConnTimeout) of
                  {ok, Socket} ->
                     gen_tcp:send(Socket, ?AgUpgradeInfo),
                     AuthInfo = eVPack:encode([1, 1000, <<"plain">>, User, Password]),
                     gen_tcp:send(Socket, AuthInfo),
                     inet:getopts(Socket, [active]),
                     AA = inet:getopts(Socket, [active]),
                     ?AgWarn(auth, "connect opt: ~p~n", [AA]),
                     case agVstCli:receiveTcpData(#recvState{}, Socket) of
                        {ok, MsgBin} ->
                           Term = eVPack:decode(MsgBin),
                           ?AgWarn(auth, "connect and auth success: ~p~n", [Term]),
                           setCurDbInfo(Socket, DbName, Protocol),
                           {ok, Socket};
                        {error, Reason} = Err ->
                           ?AgWarn(connectDb, "connect error: ~p~n", [Reason]),
                           Err
                     end;
                  {error, Reason} = Err ->
                     ?AgWarn(connectDb, "connect error: ~p~n", [Reason]),
                     Err
               end;
            ssl ->
               case ssl:connect(HostName, Port, SocketOpts, ?AgDefConnTimeout) of
                  {ok, Socket} ->
                     ssl:send(Socket, ?AgUpgradeInfo),
                     AuthInfo = eVPack:encode([1, 1000, <<"plain">>, User, Password]),
                     ssl:send(Socket, AuthInfo),
                     case agVstCli:receiveSslData(#recvState{}, Socket) of
                        {ok, MsgBin} ->
                           Term = eVPack:decode(MsgBin),
                           ?AgWarn(auth, "connect and auth success: ~p~n", [Term]),
                           setCurDbInfo(Socket, DbName, Protocol),
                           {ok, Socket};
                        {error, Reason} = Err ->
                           ?AgWarn(connectDb, "connect error: ~p~n", [Reason]),
                           Err
                     end;
                  {error, Reason} = Err ->
                     ?AgWarn(connectDb, "connect error: ~p~n", [Reason]),
                     Err
               end

   end.

-spec disConnDb(socket()) -> ok | {error, term()}.
disConnDb(Socket) ->
   case erlang:erase({'$agDbInfo', Socket}) of
      undefined ->
         ignore;
      {_DbName, _UserPassword, _Host, Protocol} ->
         case Protocol of
            tcp ->
               gen_tcp:close(Socket);
            ssl ->
               ssl:close(Socket)
         end
   end.

-spec setCurDbInfo(socket(), binary(), protocol()) -> term().
setCurDbInfo(Socket, DbName, Protocol) ->
   erlang:put({'$agDbInfo', Socket}, {DbName, Protocol}).

-spec getCurDbInfo(socket()) -> term().
getCurDbInfo(Socket) ->
   erlang:get({'$agDbInfo', Socket}).

-spec useDatabase(socket(), binary()) -> ok.
useDatabase(Socket, NewDbName) ->
   case erlang:get({'$agDbInfo', Socket}) of
      undefined ->
         ignore;
      {_DbName, Protocol} ->
         erlang:put({'$agDbInfo', Socket}, {<<"/_db/", NewDbName/binary>>, Protocol})
   end,
   ok.

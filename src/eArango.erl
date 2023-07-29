-module(eArango).

-include("agVstCli.hrl").

-export([
   start/0
   , stop/0

   %% Pools API
   , openPool/2
   , openPool/3
   , closePool/1

   %% Single Process DbAPI
   , connDb/1
   , disConnDb/1
   , getCurDbInfo/1
   , useDatabase/2

   , agencyInfo/1

]).

-export_type([dbRet/0, ps/0]).
-type(dbRet() :: {StatusCode :: non_neg_integer(), Body :: map(), Headers :: map()} | {error, term()}).
-type(ps() :: poolNameOrSocket()).

start() ->
   application:ensure_all_started(eArango).

stop() ->
   application:stop(eArango).

-spec openPool(poolName(), dbCfgs()) -> ok | {error, poolNameUsed}.
openPool(PoolName, DbCfgs) ->
   agAgencyPoolMgr:startPool(PoolName, DbCfgs, []).

-spec openPool(poolName(), dbCfgs(), agencyCfgs()) -> ok | {error, poolNameUsed}.
openPool(PoolName, DbCfgs, AgencyCfgs) ->
   agAgencyPoolMgr:startPool(PoolName, DbCfgs, AgencyCfgs).

-spec closePool(poolName()) -> ok | {error, poolNotStarted}.
closePool(PoolName) ->
   agAgencyPoolMgr:stopPool(PoolName).

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
               ntCom:syncSend(Socket, ?AgUpgradeInfo),
               AuthInfo = agVstProto:authInfo(User, Password),
               ntCom:syncSend(Socket, AuthInfo),
               case agVstCli:receiveTcpData(#recvState{}, Socket) of
                  {200, _BodyMap, _HeaderMap} ->
                     setCurDbInfo(Socket, DbName, VstSize, Protocol),
                     {ok, Socket};
                  _Err ->
                     ?AgErr(connDb_tcp, "auth error: ~p~n", [_Err]),
                     _Err
               end;
            _Err ->
               ?AgErr(connDb_tcp, "connect error: ~p~n", [_Err]),
               _Err
         end;
      ssl ->
         case ssl:connect(HostName, Port, ?AgDefSocketOpts, ?AgDefConnTimeout) of
            {ok, Socket} ->
               ssl:send(Socket, ?AgUpgradeInfo),
               AuthInfo = agVstProto:authInfo(User, Password),
               ssl:send(Socket, AuthInfo),
               case agVstCli:receiveSslData(#recvState{}, Socket) of
                  {200, _BodyMap, _HeaderMap} ->
                     setCurDbInfo(Socket, DbName, VstSize, Protocol),
                     {ok, Socket};
                  _Err ->
                     ?AgErr(connDb_ssl, "auth error: ~p~n", [_Err]),
                     _Err
               end;
            _Err ->
               ?AgErr(connDb_ssl, "connect error: ~p~n", [_Err]),
               _Err
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

agencyInfo(AgencyName) ->
   gen_server:call(AgencyName, '$SrvInfo').
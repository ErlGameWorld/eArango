-module(agAgencyPoolMgr).
-include("agVstCli.hrl").

-compile(inline).
-compile({inline_size, 128}).

-export([
   start_link/3

   , startPool/2
   , startPool/3
   , stopPool/1
   , getOneAgency/1

   , init_it/3
   , loop/2
   , system_code_change/4
   , system_continue/3
   , system_get_state/1
   , system_terminate/4

]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genActor  start %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link(module(), term(), [proc_lib:spawn_option()]) -> {ok, pid()}.
start_link(Name, Args, SpawnOpts) ->
   proc_lib:start_link(?MODULE, init_it, [Name, self(), Args], infinity, SpawnOpts).

init_it(Name, Parent, Args) ->
   case safeRegister(Name) of
      true ->
         process_flag(trap_exit, true),
         moduleInit(Parent, Args);
      {false, Pid} ->
         proc_lib:init_ack(Parent, {error, {alreadyStarted, Pid}})
   end.

-spec system_code_change(term(), module(), undefined | term(), term()) -> {ok, term()}.
system_code_change(State, _Module, _OldVsn, _Extra) ->
   {ok, State}.

-spec system_continue(pid(), [], {module(), atom(), pid(), term()}) -> ok.
system_continue(_Parent, _Debug, {Parent, State}) ->
   ?MODULE:loop(Parent, State).

-spec system_get_state(term()) -> {ok, term()}.
system_get_state(State) ->
   {ok, State}.

-spec system_terminate(term(), pid(), [], term()) -> none().
system_terminate(Reason, _Parent, _Debug, State) ->
   terminate(Reason, State).

safeRegister(Name) ->
   try register(Name, self()) of
      true -> true
   catch
      _:_ -> {false, whereis(Name)}
   end.

moduleInit(Parent, Args) ->
   case init(Args) of
      {ok, State} ->
         proc_lib:init_ack(Parent, {ok, self()}),
         ?MODULE:loop(Parent, State);
      {stop, Reason} ->
         proc_lib:init_ack(Parent, {error, Reason}),
         exit(Reason)
   end.

loop(Parent, State) ->
   receive
      {system, From, Request} ->
         sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {Parent, State});
      {'EXIT', Parent, Reason} ->
         terminate(Reason, State);
      Msg ->
         {ok, NewState} = handleMsg(Msg, State),
         ?MODULE:loop(Parent, NewState)
   end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% genActor  end %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% k-v beam cache
-define(ETS_AG_Pool, ets_ag_Pool).
-define(ETS_AG_Agency, ets_ag_Agency).

%% TODO maybe spawn_opt params need optimize
-define(agencySpec(ServerMod, ServerName, Args),
   #{
      id => ServerName
      , start => {ServerMod, start_link, [ServerName, Args, [{min_heap_size, 10240}, {min_bin_vheap_size, 524288}, {fullsweep_after, 2048}]]}
      , restart => transient
      , shutdown => infinity
      , type => worker
      , modules => [ServerMod]
}).

-spec init(Args :: term()) -> ok.
init(_Args) ->
   agVstCli:initMsgId(),
   ets:new(?ETS_AG_Pool, [named_table, set, protected]),
   ets:new(?ETS_AG_Agency, [named_table, set, protected]),
   agKvsToBeam:load(?agBeamPool, []),
   agKvsToBeam:load(?agBeamAgency, []),
   {ok, undefined}.

handleMsg({'$gen_call', From, {miStartPool, PoolName, DbCfgs, AgencyOpts}}, State) ->
   dealStart(PoolName, DbCfgs, AgencyOpts),
   gen_server:reply(From, ok),
   {ok, State};
handleMsg({'$gen_call', From, {miStopPool, Name}}, State) ->
   delaStop(Name),
   gen_server:reply(From, ok),
   {ok, State};
handleMsg(_Msg, State) ->
   ?AgErr(?MODULE, "receive unexpected  msg: ~p", [_Msg]),
   {ok, State}.

terminate(Reason, _State) ->
   ets:delete(?ETS_AG_Pool),
   ets:delete(?ETS_AG_Agency),
   agKvsToBeam:load(?agBeamPool, []),
   agKvsToBeam:load(?agBeamAgency, []),
   exit(Reason).

-spec startPool(poolName(), dbCfgs()) -> ok | {error, poolNameUsed}.
startPool(PoolName, DbCfgs) ->
   startPool(PoolName, DbCfgs, []).

-spec startPool(poolName(), dbCfgs(), agencyCfgs()) -> ok | {error, poolNameUsed}.
startPool(PoolName, DbCfgs, AgencyCfgs) ->
   case ?agBeamPool:getv(PoolName) of
      undefined ->
         gen_server:call(?agAgencyPoolMgr, {miStartPool, PoolName, DbCfgs, AgencyCfgs});
      _ ->
         {error, poolNameUsed}
   end.

-spec stopPool(poolName()) -> ok | {error, poolNotStarted}.
stopPool(PoolName) ->
   case ?agBeamPool:getv(PoolName) of
      undefined ->
         {error, poolNotStarted};
      _ ->
         gen_server:call(?agAgencyPoolMgr, {miStopPool, PoolName})
   end.

dealStart(PoolName, DbCfgs, AgencyCfgs) ->
   #dbOpts{poolSize = PoolSize, protocol = Protocol} = DbOpts = agMiscUtils:dbOpts(DbCfgs),
   AgencyOpts = agMiscUtils:agencyOpts(AgencyCfgs),
   cacheAddPool(PoolName, DbOpts),
   startChildren(PoolName, Protocol, PoolSize, AgencyOpts),
   cacheAddAgency(PoolName, PoolSize),
   case persistent_term:get(PoolName, undefined) of
      undefined ->
         IndexRef = atomics:new(1, [{signed, false}]),
         persistent_term:put(PoolName, IndexRef);
      _ ->
         ignore
   end,
   ok.

delaStop(PoolName) ->
   case ?agBeamPool:getv(PoolName) of
      undefined ->
         {error, poolNotStarted};
      #dbOpts{poolSize = PoolSize} ->
         stopChildren(agencyNames(PoolName, PoolSize)),
         cacheDelPool(PoolName),
         cacheDelAgency(PoolName),
         ok
   end.

agencyName(PoolName, Index) ->
   list_to_atom(atom_to_list(PoolName) ++ "_" ++ integer_to_list(Index)).

agencyNames(PoolName, PoolSize) ->
   [agencyName(PoolName, N) || N <- lists:seq(1, PoolSize)].

agencyMod(tcp) ->
   agTcpAgency;
agencyMod(ssl) ->
   agSslAgency;
agencyMod(_) ->
   agTcpAgency.

-spec startChildren(atom(), protocol(), poolSize(), agencyOpts()) -> ok.
startChildren(PoolName, Protocol, PoolSize, AgencyOpts) ->
   AgencyMod = agencyMod(Protocol),
   AgencyNames = agencyNames(PoolName, PoolSize),
   AgencySpecs = [?agencySpec(AgencyMod, AgencyName, {PoolName, AgencyName, AgencyOpts}) || AgencyName <- AgencyNames],
   [supervisor:start_child(agAgencyPool_sup, AgencySpec) || AgencySpec <- AgencySpecs],
   ok.

stopChildren([AgencyName | T]) ->
   case supervisor:terminate_child(agAgencyPool_sup, AgencyName) of
      ok ->
         ok;
      {error, TerReason} ->
         ?AgErr(agAgencyPoolMgr, ":terminate_child: ~p error reason: ~p ~n", [AgencyName, TerReason])
   end,
   case supervisor:delete_child(agAgencyPool_sup, AgencyName) of
      ok ->
         ok;
      {error, DelReason} ->
         ?AgErr(agAgencyPoolMgr, ":delete_child: ~p error reason: ~p ~n", [AgencyName, DelReason])
   end,
   stopChildren(T);
stopChildren([]) ->
   ok.

cacheAddPool(Key, Value) ->
   ets:insert(?ETS_AG_Pool, {Key, Value}),
   KVS = ets:tab2list(?ETS_AG_Pool),
   agKvsToBeam:load(?agBeamPool, KVS),
   ok.

cacheDelPool(Key) ->
   ets:delete(?ETS_AG_Pool, Key),
   KVS = ets:tab2list(?ETS_AG_Pool),
   agKvsToBeam:load(?agBeamPool, KVS),
   ok.

cacheAddAgency(PoolName, PoolSize) ->
   NameList = [{{PoolName, N}, agencyName(PoolName, N)} || N <- lists:seq(1, PoolSize)],
   ets:insert(?ETS_AG_Agency, NameList),
   KVS = ets:tab2list(?ETS_AG_Agency),
   agKvsToBeam:load(?agBeamAgency, KVS),
   ok.

cacheDelAgency(PoolName) ->
   ets:match_delete(?ETS_AG_Agency, {{PoolName, '_'}, '_'}),
   KVS = ets:tab2list(?ETS_AG_Agency),
   agKvsToBeam:load(?agBeamAgency, KVS),
   ok.

-spec getOneAgency(atom()) -> atom() | {error, term()}.
getOneAgency(PoolName) ->
   case ?agBeamPool:getv(PoolName) of
      undefined ->
         {error, pool_not_found};
      #dbOpts{poolSize = PoolSize} ->
         Ref = persistent_term:get(PoolName),
         AgencyIdx = atomics:add_get(Ref, 1, 1),
         case AgencyIdx >= PoolSize of
            true ->
               atomics:put(Ref, 1, 0),
               ?agBeamAgency:getv({PoolName, AgencyIdx});
            _ ->
               ?agBeamAgency:getv({PoolName, AgencyIdx})
         end
   end.

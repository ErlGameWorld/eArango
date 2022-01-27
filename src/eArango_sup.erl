-module(eArango_sup).
-include("agVstCli.hrl").
-include("eArango.hrl").

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
   supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
   SupFlags = #{strategy => one_for_one, intensity => 100, period => 3600},

   PoolMgrSpec = #{
      id => agAgencyPoolMgr,
      start => {agAgencyPoolMgr, start_link, [?agAgencyPoolMgr, [], []]},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => [agAgencyPoolMgr]
   },
   CliSupSpec = #{
      id => agAgencyPool_sup,
      start => {agAgencyPool_sup, start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => [agAgencyPool_sup]
   },
   {ok, {SupFlags, [PoolMgrSpec, CliSupSpec]}}.


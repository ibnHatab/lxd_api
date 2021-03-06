%%%-------------------------------------------------------------------
%% @doc lxd_api top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lxd_api_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSupSpecs = [#{id => lxd_api_assoc_sup,
                    start => {lxd_api_assoc_sup, start_link, []},
                    restart => transient,
                    type => supervisor},
                  #{id => lxd_api_httpc_sup,
                    start => {lxd_api_httpc_sup, start_link, []},
                    restart => transient,
                    type => supervisor}],
    {ok, {SupFlags, ChildSupSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

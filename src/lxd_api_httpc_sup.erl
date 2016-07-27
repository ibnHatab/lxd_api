%%%-------------------------------------------------------------------
%% @doc lxd_api top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lxd_api_httpc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Request, From) ->
    supervisor:start_child(?SERVER, [Request, From]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% init1([]) ->
%%     {ok,
%%         {_SupFlags = {simple_one_for_one, 1, 100},
%%             [
%%               % HTTPC Client
%%               {   undefined,                               % Id       = internal id
%%                   {lxd_api_httpc,start_link,[]},                  % StartFun = {M, F, A}
%%                   temporary,                               % Restart  = permanent | transient | temporary
%%                   2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
%%                   worker,                                  % Type     = worker | supervisor
%%                   []                                       % Modules  = [Module] | dynamic
%%               }
%%             ]
%%         }
%%     }.

init([]) ->
    SupFlags = #{ strategy => simple_one_for_one,
                  intensity => 1,
                  period => 100 },
    ChildSpecs = [#{ id => undefined,
                     start => {lxd_api_httpc, start_link, []},
                     restart => temporary,
                     type => worker,
                     shutdown => brutal_kill }],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

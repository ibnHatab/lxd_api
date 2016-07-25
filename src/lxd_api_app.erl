%%%-------------------------------------------------------------------
%% @doc lxd_api public API
%% @end
%%%-------------------------------------------------------------------

-module(lxd_api_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
-define(DEF_PORT, 3333).

start(_StartType, _StartArgs) ->
    _ListenPort = get_app_env(listen_port, ?DEF_PORT),
    lxd_api_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
    {ok, Val} -> Val;
    _ ->
        case init:get_argument(Opt) of
        [[Val | _]] -> Val;
        error       -> Default
        end
    end.

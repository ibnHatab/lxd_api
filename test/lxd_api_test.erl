-module(lxd_api_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("lxd_api.hrl").

-define(SERVER, "135.247.171.177").
-define(PORT, 8443).

ubuntu_server_test_() ->
    {setup,
     fun() ->
             {ok, _} = application:ensure_all_started(lxd_api),
             {ok, Lxd} = lxd_api:connect(?SERVER, ?PORT),
             Lxd
     end,
     fun(_) ->
             ok
     end,
     fun (Lxd) -> [ list_of_supported_apis(Lxd)

                    %% negative tests
                  , get_wrong_uri(Lxd)
                  ]
     end
    }.

list_of_supported_apis(Lxd) ->
    {ok, Rep} = lxd_api:get(Lxd, ["/"]),
    SupportedApis = lists:map(fun erlang:binary_to_list/1,
                              maps:get(<<"metadata">>, Rep)),
    ?_assert(lists:any(fun(A) -> A == ?API end, SupportedApis)).

get_wrong_uri(Lxd) ->
    {error, unsuported} = lxd_api:get(Lxd, [unsuported]),
    ?_assert(true).

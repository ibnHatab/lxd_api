-module(application_test).

-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

-define(RECV(Term), receive Term -> Term after 1000 -> error(?LINE) end).

basic_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(lxd_api) end,
     fun(_) -> ok end,
     fun(_) ->
             [
              fun() -> master() end
             ]
     end
    }.


master() ->
    ?debugMsg("Starting MASTER test").

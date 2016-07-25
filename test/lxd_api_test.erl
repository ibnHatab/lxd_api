-module(lxd_api_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("lxd_api.hrl").


-define(RECV(Term), receive Term -> Term after 1000 -> error(?LINE) end).

return_status_test_() ->
    {foreach,
     fun() ->
             {ok, _} = application:ensure_all_started(lxd_api),
             meck:new(httpc)
     end,
     fun(_) ->
             meck:validate(httpc),
             meck:unload(httpc),
             ok
     end,
     [
      fun() -> standard() end,
      fun() -> background() end,
      fun() -> error_and_failure() end
     ]
    }.

-define(SUCCES_STATUS, {"HTTP/1.1",200,"OK"}).

-define(SUCCES_HEADER, [{"date","Sun, 24 Jul 2016 14:01:07 GMT"},
                        {"content-length","2851"},
                        {"content-type","application/json"}]).

-define(SUCCES_BODY, "{
    \"type\": \"sync\",
    \"status\": \"Success\",
    \"status_code\": 200,
    \"metadata\": {}
    }").

standard() ->
    ?debugMsg("Starting standard test"),
    meck:expect(httpc, request, 4, {ok, {?SUCCES_STATUS, ?SUCCES_HEADER, ?SUCCES_BODY}}),

    Req = #http_request{server = "135.247.171.177", port = 8443, operation = 'GET',
                        resource = "", data = [], opts = opts() },
    Rep = lxd_api_httpc:service(Req).

%    ?debugMsg(io_lib:format("Format ~p~n", [Res])).

background() ->
    ?debugMsg("Starting background test").

error_and_failure() ->
    ?debugMsg("Starting error_and_failure test").

opts() ->
    Home = os:getenv("HOME"),
    Cert = Home ++ "/.config/lxc/client.crt",
    CertKey = Home ++ "/.config/lxc/client.key",
    SSLOptions = [%{verify, no},
                  {certfile, Cert},
                  {keyfile, CertKey}
                 ],
    HTTPOptions = [{ssl, SSLOptions}],
    HTTPOptions.

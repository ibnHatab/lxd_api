-module(lxd_httpc_test).

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
             _ = ?_assert(meck:validate(httpc)),
             meck:unload(httpc),
             ok
     end,
     [
      fun() -> operation_timeout() end,
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
    {ok, Rep} = lxd_api_httpc:service(Req),
    ?debugFmt(">>>> ~n~p~n", [Rep]),
    ?assertEqual(maps:get(<<"status_code">>, Rep#http_reply.json),
                 Rep#http_reply.status).

-define(ASYNC_STATUS, {"HTTP/1.1",100,"OK"}).
-define(ASYNC_BODY, "{
    \"type\": \"async\",
    \"status\": \"OK\",
    \"status_code\": 100,
    \"operation\": \"/1.0/containers/<id>\",
    \"metadata\": {}
    }").
-define(ASYNC_OPERATION_STATUS, {"HTTP/1.1",103,"OK"}).
-define(ASYNC_OPERATION_BODY, "{
    \"id\": \"a40f5541-5e98-454f-b3b6-8a51ef5dbd3c\",
    \"class\": \"websocket\",
    \"created_at\": \"2015-11-17T22:32:02.226176091-05:00\",
    \"updated_at\": \"2015-11-17T22:32:02.226176091-05:00\",
    \"status\": \"Running\",
    \"status_code\": 103,
    \"resources\": {
      \"containers\": [
        \"/1.0/containers/test\"
      ]
    },
    \"metadata\": {
      \"fds\": {
        \"0\": \"2a4a97af81529f6608dca31f03a7b7e47acc0b8dc6514496eb25e325f9e4fa6a\",
        \"control\": \"5b64c661ef313b423b5317ba9cb6410e40b705806c28255f601c0ef603f079a7\"
      }
    },
    \"may_cancel\": false,
    \"err\": \"\"
    }").


background() ->
    ?debugMsg("Starting background test"),
    meck:expect(httpc, request, 4, {meck_seq,
                                    [
                                     {ok, {?ASYNC_STATUS, ?SUCCES_HEADER, ?ASYNC_BODY}},
                                     {ok, {?ASYNC_OPERATION_STATUS, ?SUCCES_HEADER, ?ASYNC_OPERATION_BODY}},
                                     {ok, {?ASYNC_OPERATION_STATUS, ?SUCCES_HEADER, ?ASYNC_OPERATION_BODY}},
                                     {ok, {?SUCCES_STATUS, ?SUCCES_HEADER, ?SUCCES_BODY}}
                                    ]
                                   }
               ),
%    user_default:dbgon(lxd_api_httpc, async_operation),
    Req = #http_request{server = "135.247.171.177", port = 8443, operation = 'GET',
                        resource = [containers], data = [], opts = opts() },
    {ok, Rep} = lxd_api_httpc:service(Req),
    ?assertEqual(maps:get(<<"status_code">>, Rep#http_reply.json),
                 Rep#http_reply.status).


-define(ERROR_STATUS, {"HTTP/1.1",400,"NOK"}).

-define(ERROR_HEADER, [{"date","Sun, 24 Jul 2016 14:01:07 GMT"},
                        {"content-length","2851"},
                        {"content-type","application/json"}]).

-define(ERROR_BODY, "{
    \"type\": \"error\",
    \"error\": \"Failure\",
    \"error_code\": 400,
    \"metadata\": {}
    }").

error_and_failure() ->
    ?debugMsg("Starting error_and_failure test"),
    meck:expect(httpc, request, 4, {ok, {?ERROR_STATUS, ?ERROR_HEADER, ?ERROR_BODY}}),

    Req = #http_request{server = "135.247.171.177", port = 8443, operation = 'GET',
                        resource = "", data = [], opts = opts() },
    {ok, Rep} = lxd_api_httpc:service(Req),
    ?assertEqual(maps:get(<<"error_code">>, Rep#http_reply.json),
                 Rep#http_reply.status).

operation_timeout() ->
    ?debugMsg("Starting error_and_failure test"),
    meck:expect(httpc, request, 4, fun (_A,_B,_C,_D) ->
                                           timer:sleep(100),
                                           {ok, {?SUCCES_STATUS, ?SUCCES_HEADER, ?SUCCES_BODY}}
                                   end),

    Req = #http_request{server = "135.247.171.177", port = 8443, operation = 'GET',
                        resource = "", data = [], opts = opts() },

    %% user_default:dbgon(lxd_api_httpc),
    {error, operation_timeout} = lxd_api_httpc:service(Req, 10).


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

%%% -*- erlang -*-
%%%
-module(lxd_api_httpc).


-export([ensure_started/0, request/2]).

-define(RECV_TIMEOUT, 5000).
-define(API, "1.0").


ensure_started() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(hackney).

request(operation, _opts) ->
    %% http --verify no --cert ~/.config/lxc/client.crt --cert-key ~/.config/lxc/client.key https://135.247.171.177:8443/1.0/operations
    Home = os:getenv("HOME"),
    Cert = Home ++ "/.config/lxc/client.crt",
    CertKey = Home ++ "/.config/lxc/client.key",
    Host = "https://135.247.171.177:8443/1.0/containers",
    Api = "",
    Url = Host ++ Api,
    io:format(">> ~p~n",[Url]),
    SSLOptions = [%{verify, no},
                  {certfile, Cert},
                  {keyfile, CertKey}
                 ],
    HTTPOptions = [{ssl, SSLOptions}],
    io:format(">> ~p~n",[HTTPOptions]),
    { ok, {_Status, _Header, Body} } = httpc:request(get, {Url, []}, HTTPOptions, []).

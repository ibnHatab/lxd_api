lxd_api
=====

An OTP application

Build
-----

    $ rebar3 compile
    $ erl -sname rebar -pa _build/default/lib/*/ebin

LXD SOAP API
------------

    $ http --verify no --cert ~/.config/lxc/client.crt --cert-key ~/.config/lxc/client.key https://135.247.171.177:8443/1.0/operations

REST:
jiffy:decode
hackney
kivra/restclient

XML:
erlsom

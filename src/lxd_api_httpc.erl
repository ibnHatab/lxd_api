%%%-------------------------------------------------------------------
%%% @author vlad <>
%%% @copyright (C) 2016, vlad
%%% @doc
%%% Hendle HTTP request to LXD server
%%% @end
%%% Created : 22 Jul 2016 by vlad <>
%%%-------------------------------------------------------------------
-module(lxd_api_httpc).

-behaviour(gen_fsm).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([http_get/2]).
-endif.

-include("lxd_api.hrl").

%% API
-export([start_link/2, service/1]).

%% gen_fsm callbacks
-export([init/1, send_request/2, async_operation/2,
         handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).


-record(state, { original_request :: http_request(),
                 from :: pid(),
                 async_operation :: string(),
                 backoff = 300 :: pos_integer()
               }).

-define(OPERATION_TIMEOUT, 50000).
-define(OPERATION_PULL_TIMER, 10000).
-define(API, "1.0").

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Interrogate LXD server with REST operation
service(Request) ->
    service(Request, ?OPERATION_TIMEOUT).
service(Request, Timeout) ->
    {ok, Worker} = lxd_api_httpc_sup:start_child(Request, self()),
    gen_fsm:send_event(Worker, go),
    receive
        {reply, Worker, Reply} ->
            {ok, Reply};
        Any ->
            lager:error("Unexpected message from HTTPC handler", Any),
            {error, {unexpected_message_in_mailbox, Any}}
    after Timeout ->
            {error, operation_timeout}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize.
%% @end
%%--------------------------------------------------------------------
start_link(Request, From) ->
    gen_fsm:start_link(?MODULE, [Request, From], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
init([Request, From]) ->
%    process_flag(trap_exit, true),
    {ok, send_request, #state{ original_request = Request, from = From}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
send_request(go, #state{ original_request = Request, from = From, backoff = Backoff} = State) ->
    #http_request{ server = Server, port = Port, operation = Operatin,
                   resource = Resource, data = Data, opts = Opts } = Request,
    Url = build_url(Server, Port, Resource, ?API),

    {ok, #http_reply{ status = StatusCode, json = JSON} = Reply} =
        case Operatin of
            'GET'  ->  http_get(Url, Opts);
            'POST' ->  http_post(Url, Data, Opts)
        end,

    case status_code(StatusCode) of
        'Success' ->
            From ! {reply, self(), Reply},
            {stop, normal, State};
        'OperationCreated' ->
            Operation = maps:get(<<"operation">>, JSON),
            {next_state, async_operation, State#state{ async_operation = erlang:binary_to_list(Operation)}, Backoff};
        'Failure' ->
            From ! {reply, self(), Reply},
            {stop, normal, State}
    end.

async_operation(timeout, #state{ original_request = Request, from = From,
                                 async_operation = AsyncOperation, backoff = Backoff} = State) ->
    #http_request{ server = Server, port = Port, opts = Opts } = Request,
    Url = build_url(Server, Port, AsyncOperation),

    {ok, #http_reply{ status = StatusCode, json = _JSON} = Reply} = http_get(Url, Opts),
    case status_code(StatusCode) of
        'Success' ->
            From ! {reply, self(), Reply},
            {stop, normal, State};
        'Running' ->
            RetryTimer = increment(Backoff, ?OPERATION_PULL_TIMER),
            {next_state, async_operation, State#state{backoff = RetryTimer}, RetryTimer};
        'Failure' ->
            From ! {reply, self(), Reply},
            {stop, normal, State}
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% status codes: Code	Meaning
status_code(100) ->	'OperationCreated';
status_code(101) ->	'Started';
status_code(102) ->	'Stopped';
status_code(103) ->	'Running';
status_code(104) ->	'Cancelling';
status_code(105) ->	'Pending';
status_code(106) ->	'Starting';
status_code(107) ->	'Stopping';
status_code(108) ->	'Aborting';
status_code(109) ->	'Freezing';
status_code(110) ->	'Frozen';
status_code(111) ->	'Thawed';
status_code(200) ->	'Success';
status_code(400) ->	'Failure';
status_code(401) ->	'Cancelled'.


%% @doc Get request
%% http --verify no --cert ~/.config/lxc/client.crt --cert-key ~/.config/lxc/client.key https://135.247.171.177:8443/1.0/operations
http_get(Url, Opts) ->
    { ok, {Status, Header, Body} } = httpc:request(get, {Url, []}, Opts, []),

    JSON = case proplists:get_value("content-type", Header) of
               "application/json" -> jsx:decode(erlang:list_to_binary(Body), [return_maps]);
               _ -> Body
           end,
    StatusCode = check_status(Status),

    {ok, #http_reply{ status = StatusCode, json = JSON} }.

http_post(Url, Data, Opts) ->
    { ok, {Status, Header, Body} } = httpc:request(post, {Url, [], "application/json", Data}, Opts),

    JSON = case proplists:get_value("content-type", Header) of
               "application/json" -> jsx:decode(erlang:list_to_binary(Body), [return_maps]);
               _ -> Body
           end,
    StatusCode = check_status(Status),

    {ok, #http_reply{ status = StatusCode, json = JSON} }.

check_status({"HTTP/1.1", Code, _}) -> Code.

build_url(Server, Port, Resource, Api) ->
    "https://" ++ Server
        ++ ":" ++ erlang:integer_to_list(Port)
        ++ "/" ++ Api ++ encode_resource(Resource).

build_url(Server, Port, Operation) ->
    "https://" ++ Server
        ++ ":" ++ erlang:integer_to_list(Port)
        ++ Operation.

encode_resource(Resource) ->
    encode_resource(Resource, "").

encode_resource([], Acc) ->
    Acc;
encode_resource([Head|Tail], Acc) when is_atom(Head) ->
    encode_resource(Tail, Acc ++ "/" ++ erlang:atom_to_list(Head));
encode_resource([Head|Tail], Acc) when is_list(Head) ->
    encode_resource(Tail, Acc ++ "/" ++ Head).

increment(N) when is_integer(N) -> N bsl 1.
increment(N, Max) -> min(increment(N), Max).

-ifdef(TEST).

url_encoder_test_() ->
    [
      %% resource
      ?_assert(encode_resource([op, "name", sel]) =:= "/op/name/sel"),
      ?_assert(encode_resource([op, "name", sel, "name"]) =:= "/op/name/sel/name"),
      %% uri
      ?_assert(build_url("135.247.171.177", 8443, [operations], ?API) =:= "https://135.247.171.177:8443/1.0/operations"),
      ?_assert(build_url("135.247.171.177", 8443, [operations, "1234"], ?API) =:= "https://135.247.171.177:8443/1.0/operations/1234"),
      ?_assert(build_url("135.247.171.177", 8443, "/1.0/operations/1234") =:= "https://135.247.171.177:8443/1.0/operations/1234")
    ].

-endif.


%% opts() ->
%%     Home = os:getenv("HOME"),
%%     Cert = Home ++ "/.config/lxc/client.crt",
%%     CertKey = Home ++ "/.config/lxc/client.key",
%%     SSLOptions = [%{verify, no},
%%                   {certfile, Cert},
%%                   {keyfile, CertKey}
%%                  ],
%%     HTTPOptions = [{ssl, SSLOptions}],
%%     HTTPOptions.

%% test_query() ->
%% %    application:ensure_all_started(lxd_api),
%%     Req = #http_request{server = "135.247.171.177", port = 8443, operation = 'GET',
%%                         resource = "", data = [], opts = opts() },
%%     lxd_api_httpc:service(Req).

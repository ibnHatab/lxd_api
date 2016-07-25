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
-endif.

-include("lxd_api.hrl").

%% API
-export([start_link/1, service/1]).
-export_record([http_request, http_reply]).


%% gen_fsm callbacks
-export([init/1, send_request/2, send_request/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).


-record(state, { original_request :: http_request(),
                 partial_reply :: http_reply()
               }).

-define(RECV_TIMEOUT, 5000).
-define(API, "1.0").

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Interrogate LXD server with REST operation
service(Request) ->
    lxd_api_sup:start_child(Request).


%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Request) ->
    gen_fsm:start_link(?MODULE, [Request], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Request]) ->
    process_flag(trap_exit, true),
    {ok, send_request, #state{ original_request = Request}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec send_request(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
send_request(_Event, #state{ original_request = Request} = State) ->
    #http_request{ server = Server, port = Port, operation = Operatin,
                   resource = Resource, data = Data, opts = Opts } = Request,
    Url = build_url(Server, Port, Resource),
    {ok, Status, Header, Body} =
        case Operatin of
            'GET' ->
                http_get(Url, Opts);
            'PUT' ->
                http_put(Url, Data, Opts)
        end,



    {next_state, send_request, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec send_request(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
send_request(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, send_request, State}.

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


%% @doc Get request
%% http --verify no --cert ~/.config/lxc/client.crt --cert-key ~/.config/lxc/client.key https://135.247.171.177:8443/1.0/operations
http_get(Url, Opts) ->
    { ok, {Status, Header, Body} } = httpc:request(get, {Url, []}, Opts, []),

    {ok, #http_reply{}}.

http_put(Url, Data, Opts) ->
    { ok, {Status, Header, Body} } = httpc:request(get, {Url, []}, Opts, []),
    {ok, #http_reply{}}.


build_url(Server, Port, Resource) ->
    Url = "https://" ++ Server
        ++ ":" ++ erlang:integer_to_list(Port)
        ++ "/" ++ ?API ++ encode_resource(Resource),
    Url.

encode_resource(Resource) ->
    encode_resource(Resource, "").

encode_resource([], Acc) ->
    Acc;
encode_resource([Head|Tail], Acc) when is_atom(Head) ->
    encode_resource(Tail, Acc ++ "/" ++ erlang:atom_to_list(Head));
encode_resource([Head|Tail], Acc) when is_list(Head) ->
    encode_resource(Tail, Acc ++ "/" ++ Head).


-ifdef(TEST).

url_encoder_test_() ->
    [
      %% resource
      ?_assert(encode_resource([op, "name", sel]) =:= "/op/name/sel"),
      ?_assert(encode_resource([op, "name", sel, "name"]) =:= "/op/name/sel/name"),
      %% uri
      ?_assert(build_url("135.247.171.177", 8443, [operations]) =:= "https://135.247.171.177:8443/1.0/operations"),
      ?_assert(build_url("135.247.171.177", 8443, [operations, "1234"]) =:= "https://135.247.171.177:8443/1.0/operations/1234")
    ].


-endif.

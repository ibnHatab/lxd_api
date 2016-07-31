%%%-------------------------------------------------------------------
%%% @author vlad <>
%%% @copyright (C) 2016, vlad
%%% @doc
%%%
%%% @end
%%% Created : 21 Jul 2016 by vlad <>
%%%-------------------------------------------------------------------
-module(lxd_api).

-behaviour(gen_server).

-include("lxd_api.hrl").

%% API
-export([start_link/3, connect/2, connect/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_FINGER_PRINT, <<"">>).

-record(state, {server       :: string(),
                port         :: pos_integer(),
                finger_print :: binary(),
                http_opts    :: list()
               }).


%%%===================================================================
%%% API
%%%===================================================================
%% @doc Create LXC serer connection
connect(Server, Port) ->
    connect(Server, Port, ?DEFAULT_FINGER_PRINT).

connect(Server, Port, Fingerprint) ->
    lxd_api_sup:start_child(Server, Port, Fingerprint).

%% Resource API structure
%% /
%% /1.0
%% /1.0/certificates
%% /1.0/certificates/<fingerprint>
%% /1.0/containers
%% /1.0/containers/<name>
%% /1.0/containers/<name>/exec
%% /1.0/containers/<name>/files
%% /1.0/containers/<name>/snapshots
%% /1.0/containers/<name>/snapshots/<name>
%% /1.0/containers/<name>/state
%% /1.0/containers/<name>/logs
%% /1.0/containers/<name>/logs/<logfile>
%% /1.0/events
%% /1.0/images
%% /1.0/images/<fingerprint>
%% /1.0/images/<fingerprint>/export
%% /1.0/images/aliases
%% /1.0/images/aliases/<name>
%% /1.0/networks
%% /1.0/networks/<name>
%% /1.0/operations
%% /1.0/operations/<uuid>
%% /1.0/operations/<uuid>/wait
%% /1.0/operations/<uuid>/websocket
%% /1.0/profiles
%% /1.0/profiles/<name>
%%
%% Ref:
%%  https://github.com/lxc/lxd/blob/master/doc/rest-api.md

'GET'(Lxd, Endpoint) ->
    gen_server:call(Lxd, {get, Endpoint}).

'PUT'(Lxd, Endpoint, Data) ->
    gen_server:call(Lxd, {put, Endpoint, Data}).

'PATCH'(Lxd, Endpoint, Data) ->
    gen_server:call(Lxd, {patch, Endpoint, Data}).

'POST'(Lxd, Endpoint, Data) ->
    gen_server:call(Lxd, {post, Endpoint, Data}).

'DELETE'(Lxd, Endpoint) ->
    gen_server:call(Lxd, {delete, Endpoint}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Server, Port, Fingerprint) ->
    gen_server:start_link(?MODULE, [Server, Port, Fingerprint], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Server, Port, Fingerprint]) ->
    Home = os:getenv("HOME"),
    Cert = Home ++ "/.config/lxc/client.crt",
    CertKey = Home ++ "/.config/lxc/client.key",
    SSLOptions = [%{verify, no},
                  {certfile, Cert},
                  {keyfile, CertKey}
                 ],
    HTTPOptions = [{ssl, SSLOptions}],

    {ok, #state{server = Server,
                port = Port,
                finger_print = Fingerprint,
                http_opts = HTTPOptions
               }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages


handle_call( {get, [certificates]}, _From, State) ->
    {reply, ok, State};

handle_call({get, [certificates, Fingerprint]}, _From, State)->
    {reply, ok, State};


%% handle_call({get, [containers]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [containers, Name]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [containers, Name, exec]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [containers, Name, files]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [containers, Name, snapshots]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [containers, Name, snapshots, Name]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [containers, Name, state]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [containers, Name, logs]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [containers, Name, logs, Logfile]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [events]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [images]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [images, Fingerprint]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [images, Fingerprint, export]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [images, aliases]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [images, aliases, Name]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [networks]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [networks, Name]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [operations]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [operations, Uuid]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [operations, Uuid, wait]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [operations, Uuid, websocket]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [profiles]}, _From, State)-> {reply, ok, State};
%% handle_call({get, [profiles, Name]}, _From, State)->     {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = {error, notsupported},
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% config - Manage configuration.
%% copy   - Copy containers within or in between lxd instances.
%% delete - Delete containers or container snapshots.
%% exec   - Execute the specified command in a container.
%% file   - Manage files on a container.
%% image  - Manipulate container images.
%% info   - List information on LXD servers and containers.
%% launch - Launch a container from a particular image.
%% list   - Lists the available resources.
%% move   - Move containers within or in between lxd instances.
%% profile - Manage configuration profiles.
%% remote - Manage remote LXD servers.
%% restart - Changes state of one or more containers to restart.
%% restore -  Set  the  current state of a resource back to a snapshot.
%% snapshot - Create a read-only snapshot of a container.
%% start  - Changes state of one or more containers to start.
%% stop   - Changes state of one or more containers to stop.

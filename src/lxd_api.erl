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
-export([start_link/2, connect/2, authenticate/1,
         get/2, put/3, patch/3, post/3, delete/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

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
    lxd_api_assoc_sup:start_child(Server, Port).

authenticate(Fingerprint) ->
    ok.


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

get(Lxd, Endpoint) ->
    gen_server:call(Lxd, {get, Endpoint}).

put(Lxd, Endpoint, Data) ->
    gen_server:call(Lxd, {put, Endpoint, Data}).

patch(Lxd, Endpoint, Data) ->
    gen_server:call(Lxd, {patch, Endpoint, Data}).

post(Lxd, Endpoint, Data) ->
    gen_server:call(Lxd, {post, Endpoint, Data}).

delete(Lxd, Endpoint) ->
    gen_server:call(Lxd, {delete, Endpoint}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Server, Port) ->
    gen_server:start_link(?MODULE, [Server, Port], []).

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
init([Server, Port]) ->
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
                http_opts = HTTPOptions
               }}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({get, Resource}, _From,
             #state{server=Server,port=Port,http_opts=HTTPOptions} = State) ->
    Req = #http_request{operation='GET',resource=Resource,
                        server=Server,port=Port,opts=HTTPOptions},
    Suported = api_support(get, Resource),
    if Suported ->
            case decode_status(lxd_api_httpc:service(Req)) of
                {ok, Data} ->
                    {reply, {ok, Data}, State};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end;
       true ->
            {reply, {error, unsuported}, State}
    end;

handle_call({put, Resource, Data}, _From, State)->
    {reply, ok, State};

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

%% Private functions
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Describe allowed operations for LXD API 1.0
api_support(get, Resource) ->
    case Resource of
        ["/"]                              -> true;
        [?API]                             -> true;
        [certificates]                     -> true;
        [certificates,_Fingerprint]        -> true;
        [containers]                       -> true;
        [containers,_Name]                 -> true;
        [containers,_Name,state]           -> true;
        [containers,_Name,files]           -> true;
        [containers,_Name,snapshots]       -> true;
        [containers,_Name,snapshots,_Name] -> true;
        [containers,_Name,logs]            -> true;
        [containers,_Name,logs,_Logfile]   -> true;
        [events]                           -> true;
        [images]                           -> true;
        [images,_Fingerprint]              -> true;
        [images,_Fingerprint,export]       -> true;
        [images,aliases]                   -> true;
        [images,aliases,_Name]             -> true;
        [networks]                         -> true;
        [networks,_Name]                   -> true;
        [operations]                       -> true;
        [operations,_Uuid]                 -> true;
        [operations,_Uuid,wait]            -> true;
        [operations,_Uuid,websocket]       -> true;
        [profiles]                         -> true;
        [profiles,_Name]                   -> true;
        _ -> false
    end;
api_support(put, Resource) ->
    case Resource of
        [?API]                             -> true;
        [containers,_Name]                 -> true;
        [containers,_Name,state]           -> true;
        [images,_Fingerprint]              -> true;
        [images,aliases,_Name]             -> true;
        [profiles,_Name]                   -> true;
        _                                  ->
            false
    end;
api_support(patch, Resource) ->
    case Resource of
        [?API]                             -> true;
        [containers,_Name]                 -> true;
        [images,_Fingerprint]              -> true;
        [images,aliases,_Name]             -> true;
        [profiles,_Name]                   -> true;
        _ ->
            false
    end;
api_support(post, Resource) ->
    case Resource of
        [certificates]                     -> true;
        [containers]                       -> true;
        [containers,_Name]                 -> true;
        [containers,_Name,files]           -> true;
        [containers,_Name,snapshots]       -> true;
        [containers,_Name,snapshots,_Name] -> true;
        [containers,_Name,exec]            -> true;
        [images]                           -> true;
        [images,_Fingerprint,export]       -> true;
        [images,aliases]                   -> true;
        [images,aliases,_Name]             -> true;
        [profiles]                         -> true;
        [profiles,_Name]                   -> true;
        _ ->
            false
    end;
api_support(delete, Resource) ->
    case Resource of
        [certificates,_Fingerprint]        -> true;
        [containers,_Name]                 -> true;
        [containers,_Name,snapshots,_Name] -> true;
        [containers,_Name,logs,_Logfile]   -> true;
        [images,_Fingerprint]              -> true;
        [images,aliases,_Name]             -> true;
        [operations,_Uuid]                 -> true;
        [profiles,_Name]                   -> true;
        _ ->
            false
    end.


decode_status({ok, #http_reply{status = Status, json = Data}}) ->
    %% Unify API error with http error
    case lxd_api_httpc:status_code(Status) of
        'Success' -> {ok, Data};
        ErrorCode -> {error, ErrorCode}
    end;
decode_status({error, Error}) ->
    {error, Error}.


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

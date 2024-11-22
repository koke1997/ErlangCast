-module(peer_discovery_service).

-behaviour(gen_server).

-export([start/0, stop/0, handle_message/1, broadcast_message/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

% Start the peer discovery process
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Stop the peer discovery process
stop() ->
    gen_server:stop(?MODULE).

% Handle incoming peer discovery messages
handle_message(Msg) ->
    gen_server:cast(?MODULE, {handle_message, Msg}).

% Broadcast peer discovery messages
broadcast_message(Msg) ->
    gen_server:cast(?MODULE, {broadcast_message, Msg}).

% Initialize the gen_server
init([]) ->
    {ok, #state{}}.

% Handle synchronous calls
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

% Handle asynchronous messages
handle_cast({handle_message, Msg}, State) ->
    io:format("Received peer discovery message: ~p~n", [Msg]),
    {noreply, State};
handle_cast({broadcast_message, Msg}, State) ->
    io:format("Broadcasting peer discovery message: ~p~n", [Msg]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

% Handle other messages
handle_info(_Info, State) ->
    error_logger:error_msg("Unexpected info received: ~p~n", [_Info]),
    {noreply, State}.

% Terminate the gen_server
terminate(_Reason, _State) ->
    ok.

% Handle code changes
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

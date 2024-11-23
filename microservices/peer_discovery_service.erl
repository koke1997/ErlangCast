-module(peer_discovery_service).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

% Start the peer discovery process
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Stop the peer discovery process
stop() ->
    gen_server:stop(?MODULE).

% Initialize the gen_server
init([]) ->
    {ok, #state{}}.

% Handle synchronous calls
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

% Handle asynchronous messages
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

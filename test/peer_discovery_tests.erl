-module(peer_discovery_tests).

-include_lib("eunit/include/eunit.hrl").

start_test() ->
    {ok, Pid} = gen_server:start_link({local, test_server}, test_server, [], []),
    ?assertEqual(ok, peer_discovery:start()),
    gen_server:stop(Pid).

stop_test() ->
    {ok, Pid} = gen_server:start_link({local, test_server}, test_server, [], []),
    ?assertEqual(ok, peer_discovery:stop()),
    gen_server:stop(Pid).

handle_message_test() ->
    ?assertEqual(ok, peer_discovery:handle_message("Test message")),
    ?assertEqual(ok, peer_discovery:handle_message("")),
    ?assertException(error, _, peer_discovery:handle_message(123)).

broadcast_message_test() ->
    ?assertEqual(ok, peer_discovery:broadcast_message("Test message")),
    ?assertEqual(ok, peer_discovery:broadcast_message("")),
    ?assertException(error, _, peer_discovery:broadcast_message(123)).

start_error_test() ->
    {ok, Pid} = gen_server:start_link({local, test_server}, test_server, [], []),
    ?assertEqual({error, already_started}, peer_discovery:start()),
    gen_server:stop(Pid).

stop_error_test() ->
    {ok, Pid} = gen_server:start_link({local, test_server}, test_server, [], []),
    ?assertEqual({error, not_started}, peer_discovery:stop()),
    gen_server:stop(Pid).

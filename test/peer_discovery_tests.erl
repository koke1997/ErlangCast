-module(peer_discovery_tests).

-include_lib("eunit/include/eunit.hrl").

start_test() ->
    ?assertEqual(ok, peer_discovery:start()),
    ?assertEqual({error, already_started}, peer_discovery:start()).

stop_test() ->
    ?assertEqual(ok, peer_discovery:stop()),
    ?assertEqual({error, not_started}, peer_discovery:stop()).

handle_message_test() ->
    ?assertEqual(ok, peer_discovery:handle_message("Test message")).

broadcast_message_test() ->
    ?assertEqual(ok, peer_discovery:broadcast_message("Test message")).

-module(peer_discovery_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("meck/include/meck.hrl").

start_test() ->
    meck:new(gen_server, [unstick, passthrough]),
    meck:expect(gen_server, start_link, fun(_, _, _) -> {ok, self()} end),
    ?assertEqual(ok, peer_discovery:start()),
    meck:unload(gen_server).

stop_test() ->
    meck:new(gen_server, [unstick, passthrough]),
    meck:expect(gen_server, stop, fun(_) -> ok end),
    ?assertEqual(ok, peer_discovery:stop()),
    meck:unload(gen_server).

handle_message_test() ->
    ?assertEqual(ok, peer_discovery:handle_message("Test message")),
    ?assertEqual(ok, peer_discovery:handle_message("")),
    ?assertException(error, _, peer_discovery:handle_message(123)).

broadcast_message_test() ->
    ?assertEqual(ok, peer_discovery:broadcast_message("Test message")),
    ?assertEqual(ok, peer_discovery:broadcast_message("")),
    ?assertException(error, _, peer_discovery:broadcast_message(123)).

start_error_test() ->
    meck:new(gen_server, [unstick, passthrough]),
    meck:expect(gen_server, start_link, fun(_, _, _) -> {error, already_started} end),
    ?assertEqual({error, already_started}, peer_discovery:start()),
    meck:unload(gen_server).

stop_error_test() ->
    meck:new(gen_server, [unstick, passthrough]),
    meck:expect(gen_server, stop, fun(_) -> {error, not_started} end),
    ?assertEqual({error, not_started}, peer_discovery:stop()),
    meck:unload(gen_server).

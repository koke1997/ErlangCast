-module(reliable_transmission_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("meck/include/meck.hrl").

send_data_test() ->
    meck:new(self, [unstick, passthrough]),
    meck:expect(self, fun() -> self() end),
    Peer = self(),
    Data = <<"Test data">>,
    ?assertEqual({ok, data_sent}, reliable_transmission:send_data(Peer, Data)),
    meck:unload(self).

receive_data_test() ->
    meck:new(self, [unstick, passthrough]),
    meck:expect(self, fun() -> self() end),
    DataHandler = fun(Data) ->
        ?assertEqual(<<"Test data">>, Data)
    end,
    self() ! {data, <<"Test data">>},
    reliable_transmission:receive_data(DataHandler),
    meck:unload(self).

send_data_with_retry_test() ->
    meck:new(self, [unstick, passthrough]),
    meck:expect(self, fun() -> self() end),
    Peer = self(),
    Data = <<"Test data">>,
    ?assertEqual({ok, data_sent}, reliable_transmission:send_data(Peer, Data)),
    meck:unload(self).

receive_data_with_retry_test() ->
    meck:new(self, [unstick, passthrough]),
    meck:expect(self, fun() -> self() end),
    DataHandler = fun(Data) ->
        ?assertEqual(<<"Test data">>, Data)
    end,
    self() ! {data, <<"Test data">>},
    reliable_transmission:receive_data(DataHandler),
    meck:unload(self).

send_data_invalid_peer_test() ->
    InvalidPeer = undefined,
    Data = <<"Test data">>,
    ?assertException(error, _, reliable_transmission:send_data(InvalidPeer, Data)).

send_data_timeout_test() ->
    meck:new(self, [unstick, passthrough]),
    meck:expect(self, fun() -> self() end),
    Peer = self(),
    Data = <<"Test data">>,
    ?assertEqual({error, retry_limit_reached}, reliable_transmission:send_data(Peer, Data, 5)),
    meck:unload(self).

receive_data_invalid_input_test() ->
    meck:new(self, [unstick, passthrough]),
    meck:expect(self, fun() -> self() end),
    DataHandler = fun(Data) ->
        ?assertEqual(<<"Test data">>, Data)
    end,
    self() ! {invalid_data, <<"Test data">>},
    ?assertException(error, _, reliable_transmission:receive_data(DataHandler)),
    meck:unload(self).

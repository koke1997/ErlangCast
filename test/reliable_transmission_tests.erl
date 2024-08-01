-module(reliable_transmission_tests).

-include_lib("eunit/include/eunit.hrl").

send_data_test() ->
    Peer = self(),
    Data = <<"Test data">>,
    ?assertEqual({ok, data_sent}, reliable_transmission:send_data(Peer, Data)).

receive_data_test() ->
    DataHandler = fun(Data) ->
        ?assertEqual(<<"Test data">>, Data)
    end,
    self() ! {data, <<"Test data">>},
    reliable_transmission:receive_data(DataHandler).

send_data_with_retry_test() ->
    Peer = self(),
    Data = <<"Test data">>,
    ?assertEqual({ok, data_sent}, reliable_transmission:send_data(Peer, Data)).

receive_data_with_retry_test() ->
    DataHandler = fun(Data) ->
        ?assertEqual(<<"Test data">>, Data)
    end,
    self() ! {data, <<"Test data">>},
    reliable_transmission:receive_data(DataHandler).

send_data_invalid_peer_test() ->
    InvalidPeer = undefined,
    Data = <<"Test data">>,
    ?assertException(error, _, reliable_transmission:send_data(InvalidPeer, Data)).

send_data_timeout_test() ->
    Peer = self(),
    Data = <<"Test data">>,
    ?assertEqual({error, retry_limit_reached}, reliable_transmission:send_data(Peer, Data, 5)).

receive_data_invalid_input_test() ->
    DataHandler = fun(Data) ->
        ?assertEqual(<<"Test data">>, Data)
    end,
    self() ! {invalid_data, <<"Test data">>},
    ?assertException(error, _, reliable_transmission:receive_data(DataHandler)).

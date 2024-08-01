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

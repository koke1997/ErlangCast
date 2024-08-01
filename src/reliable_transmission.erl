-module(reliable_transmission).

-export([send_data/2, receive_data/1]).

-define(RETRY_LIMIT, 5).
-define(ACK_TIMEOUT, 1000).

send_data(Peer, Data) ->
    send_data(Peer, Data, 0).

send_data(_Peer, _Data, ?RETRY_LIMIT) ->
    {error, retry_limit_reached};
send_data(Peer, Data, RetryCount) ->
    % Send data to peer
    Peer ! {data, Data},
    % Wait for acknowledgment
    receive
        {ack, Peer} ->
            {ok, data_sent};
        after ?ACK_TIMEOUT ->
            % Retry sending data
            send_data(Peer, Data, RetryCount + 1)
    end.

receive_data(DataHandler) ->
    receive
        {data, Data} ->
            % Process received data
            DataHandler(Data),
            % Send acknowledgment
            self() ! {ack, self()},
            receive_data(DataHandler)
    end.

-module(reliable_transmission).

-export([send_data/2, receive_data/1, handle_transmission_error/2]).

-define(RETRY_LIMIT, 5).
-define(ACK_TIMEOUT, 1000).

send_data(Peer, Data) ->
    send_data(Peer, Data, 0).

send_data(_Peer, _Data, ?RETRY_LIMIT) ->
    handle_transmission_error(_Peer, retry_limit_exceeded);

send_data(Peer, Data, RetryCount) ->
    try
        Peer ! {data, self(), Data},
        receive
            {ack, Peer} ->
                {ok, sent}
        after ?ACK_TIMEOUT ->
            case handle_transmission_error(Peer, {timeout, RetryCount}) of
                retry -> 
                    send_data(Peer, Data, RetryCount + 1);
                Error -> 
                    Error
            end
        end
    catch
        error:Reason ->
            handle_transmission_error(Peer, Reason)
    end.

receive_data(Timeout) ->
    receive
        {data, Sender, Data} ->
            case send_ack(Sender) of
                ok ->
                    {ok, Data};
                Error ->
                    handle_transmission_error(Sender, Error)
            end;
        {error, Sender, Reason} ->
            handle_transmission_error(Sender, Reason)
    after Timeout ->
        handle_transmission_error(undefined, timeout)
    end.

send_ack(Sender) ->
    try
        Sender ! {ack, self()},
        ok
    catch
        _:Error -> Error
    end.

handle_transmission_error(Peer, Reason) ->
    error_logger:error_msg("Transmission error with peer ~p: ~p~n", [Peer, Reason]),
    case Reason of
        {timeout, RetryCount} when RetryCount < ?RETRY_LIMIT -> 
            retry;
        timeout -> 
            {error, {timeout, Peer}};
        retry_limit_exceeded ->
            {error, {retry_limit_exceeded, Peer}};
        _ -> 
            {error, {transmission_failed, Peer, Reason}}
    end.

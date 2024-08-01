# ErlangCast
ErlangCast is a P2P video streaming server built with Erlang, leveraging its strengths in concurrency and distributed programming. Features include peer discovery, video chunking, reliable data transmission, and fault-tolerant architecture. Ideal for real-time streaming and decentralized applications.

## Peer Discovery
The peer discovery feature allows peers to find each other in the network, enabling efficient communication and data sharing.

### How to Use Peer Discovery
1. Start the peer discovery process by calling the `peer_discovery:start/0` function.
2. Handle incoming peer discovery messages using the `peer_discovery:handle_message/1` function.
3. Broadcast peer discovery messages using the `peer_discovery:broadcast_message/1` function.
4. Stop the peer discovery process by calling the `peer_discovery:stop/0` function.

### Example
```erlang
% Start the peer discovery process
peer_discovery:start().

% Handle an incoming peer discovery message
peer_discovery:handle_message("Hello, peer!").

% Broadcast a peer discovery message
peer_discovery:broadcast_message("Hello, peers!").

% Stop the peer discovery process
peer_discovery:stop().
```

## Video Chunking
The video chunking feature allows the system to divide video files into smaller chunks for transmission, enabling efficient streaming and data handling.

### How to Use Video Chunking
1. Chunk a video file by calling the `video_chunking:chunk_video/2` function with the video file path and chunk size as arguments.
2. Retrieve a specific chunk of a video file using the `video_chunking:get_chunk/2` function with the video file path and chunk index as arguments.

### Example
```erlang
% Chunk a video file into 1MB chunks
video_chunking:chunk_video("path/to/video.mp4", 1048576).

% Retrieve the first chunk of the video file
{ok, Chunk} = video_chunking:get_chunk("path/to/video.mp4", 1).
```

## Reliable Data Transmission
The reliable data transmission feature ensures that data is transmitted reliably between peers, handling packet loss and retransmissions.

### How to Use Reliable Data Transmission
1. Send data reliably by calling the `reliable_transmission:send_data/2` function with the peer and data as arguments.
2. Receive data reliably by calling the `reliable_transmission:receive_data/1` function with a data handler function as an argument.

### Example
```erlang
% Send data reliably to a peer
reliable_transmission:send_data(Peer, "Hello, peer!").

% Define a data handler function
DataHandler = fun(Data) ->
    io:format("Received data: ~p~n", [Data])
end.

% Receive data reliably
reliable_transmission:receive_data(DataHandler).
```

## Fault-Tolerant Architecture
The fault-tolerant architecture ensures that the system can handle failures gracefully and continue operating. This includes error handling and recovery mechanisms in various modules.

### Fault-Tolerant Features
1. **Peer Discovery**: The `peer_discovery` module includes error handling and recovery mechanisms for peer discovery failures.
2. **Reliable Transmission**: The `reliable_transmission` module includes error handling for retry limit reached and logs the error.
3. **Video Chunking**: The `video_chunking` module includes error handling for file operations and chunking process.
4. **System Configuration**: The `sys.config` file includes fault-tolerance configurations for the system.

### Example
```erlang
% Example of error handling in peer discovery
case peer_discovery:start() of
    ok -> io:format("Peer discovery started successfully~n");
    {error, Reason} -> io:format("Failed to start peer discovery: ~p~n", [Reason])
end.

% Example of error handling in reliable transmission
case reliable_transmission:send_data(Peer, "Hello, peer!") of
    {ok, data_sent} -> io:format("Data sent successfully~n");
    {error, retry_limit_reached} -> io:format("Failed to send data: retry limit reached~n")
end.

% Example of error handling in video chunking
case video_chunking:chunk_video("path/to/video.mp4", 1048576) of
    {ok, Chunks} -> io:format("Video chunked successfully~n");
    {error, Reason} -> io:format("Failed to chunk video: ~p~n", [Reason])
end.
```

## Testing
The testing section provides instructions for running unit tests and integration tests to ensure the system works as expected.

### Running Unit Tests
To run the unit tests, use the following command:
```sh
rebar3 eunit
```

### Running Integration Tests
To run the integration tests, use the following command:
```sh
rebar3 ct
```

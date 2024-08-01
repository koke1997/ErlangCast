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

## Development Environment Setup
To set up the development environment for ErlangCast, follow these steps:

1. **Install Erlang/OTP**: Ensure you have Erlang/OTP installed on your system. You can download it from the [official Erlang website](https://www.erlang.org/downloads).

2. **Install Rebar3**: Rebar3 is the build tool used for this project. You can install it by following the instructions on the [Rebar3 website](https://rebar3.org/docs/getting-started).

3. **Clone the Repository**: Clone the ErlangCast repository to your local machine:
   ```sh
   git clone https://github.com/koke1997/ErlangCast.git
   cd ErlangCast
   ```

4. **Fetch Dependencies**: Use Rebar3 to fetch the project dependencies:
   ```sh
   rebar3 get-deps
   ```

5. **Compile the Project**: Compile the project using Rebar3:
   ```sh
   rebar3 compile
   ```

## Contributing
We welcome contributions to ErlangCast! To contribute, follow these steps:

1. **Fork the Repository**: Fork the ErlangCast repository on GitHub.

2. **Create a Branch**: Create a new branch for your feature or bugfix:
   ```sh
   git checkout -b my-feature-branch
   ```

3. **Make Changes**: Make your changes to the codebase.

4. **Commit Changes**: Commit your changes with a descriptive commit message:
   ```sh
   git commit -am "Add new feature"
   ```

5. **Push Changes**: Push your changes to your forked repository:
   ```sh
   git push origin my-feature-branch
   ```

6. **Create a Pull Request**: Create a pull request on the original repository, describing your changes and the problem they solve.

## Configuration Files
ErlangCast uses configuration files to manage various settings. Here are detailed explanations of the configuration files:

### `config/rebar.config`
The `rebar.config` file specifies the project dependencies and compiler options. Here is an example:
```erlang
{erl_opts, [debug_info]}.

{deps, [
    {cowboy, "2.9.0"},
    {jsx, "2.11.0"},
    {ffmpeg, "4.4.0"},
    {eunit, "2.3.0"}
]}.
```
- `erl_opts`: Compiler options for Erlang.
- `deps`: List of project dependencies with their versions.

### `config/sys.config`
The `sys.config` file contains runtime configuration for the Erlang system. Here is an example:
```erlang
[
  {kernel, [
    {inet_dist_listen_min, 9100},
    {inet_dist_listen_max, 9155},
    {error_logger, {file, "log/error.log"}}
  ]},
  {sasl, [
    {sasl_error_logger, {file, "log/sasl-error.log"}},
    {errlog_type, error}
  ]},
  {peer_discovery, [
    {port_range, {9200, 9255}},
    {broadcast_interval, 5000},
    {max_peers, 50}
  ]},
  {fault_tolerance, [
    {retry_limit, 5},
    {ack_timeout, 1000},
    {max_retries, 3}
  ]}
].
```
- `kernel`: Configuration for the Erlang kernel, including network settings and error logging.
- `sasl`: Configuration for the SASL (System Architecture Support Libraries) application, including error logging.
- `peer_discovery`: Configuration for the peer discovery module, including port range, broadcast interval, and maximum peers.
- `fault_tolerance`: Configuration for fault tolerance settings, including retry limits and acknowledgment timeouts.

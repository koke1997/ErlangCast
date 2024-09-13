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

## Scala Connector
The Scala connector enables communication between the Erlang server and the Scala videoserver, leveraging Akka for actor-based communication. Scala is also used to raise the needed frontend to interact with the videoserver.

### How to Use Scala Connector
1. Set up Scala and necessary dependencies.
2. Use the `ScalaConnector` object to send and receive messages between Scala and Erlang.

### Example
```scala
import ScalaConnector._

object Main extends App {
  // Send a message to Erlang
  sendToErlang("Hello, Erlang!")

  // Receive a message from Erlang
  val message = Await.result(receiveFromErlang(), 5.seconds)
  println(s"Received message from Erlang: $message")
}
```

## Setting Up and Running the Scala Frontend
To set up and run the Scala frontend, follow these steps:

1. Ensure Scala and sbt are installed on your system.
2. Navigate to the project directory.
3. Compile the Scala code using sbt:
   ```sh
   sbt compile
   ```
4. Run the Scala frontend:
   ```sh
   sbt run
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

### Running Test Coverage
To run the test coverage, use the following command:
```sh
rebar3 as test cover
```

## Automated Testing Workflow
The automated testing workflow uses GitHub Actions to run tests automatically after a pull request is made. This ensures that the tests are always up-to-date and provides immediate feedback on the status of the code.

### How to View Test Results
1. Navigate to the "Actions" tab in your GitHub repository.
2. Select the workflow run you want to view.
3. Check the logs for detailed information about the test results and verbose output.

### Example Workflow File
The following is an example of a GitHub Actions workflow file (`.github/workflows/ci.yml`) that automates the test execution:
```yaml
name: CI

on:
  pull_request:
    branches:
      - main

jobs:
  build:
    runs-on: ubuntu-latest

    steps:
      - name: Checkout code
        uses: actions/checkout@v2

      - name: Set up Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: 24.x
          rebar3-version: 3.16.1

      - name: Install dependencies
        run: rebar3 get-deps

      - name: Run unit tests
        run: rebar3 eunit --verbose

      - name: Run integration tests
        run: rebar3 ct --verbose
```

## Running the Project Locally

### On Linux

To run the project locally on Linux, follow these steps:

1. Open a terminal and navigate to the project directory.
2. Run the installation script to install all dependencies:
   ```sh
   ./scripts/install_dependencies.sh
   ```
3. Run the project using the local run script:
   ```sh
   ./scripts/run_local.sh
   ```

### On Windows

To run the project locally on Windows, follow these steps:

1. Open a command prompt and navigate to the project directory.
2. Run the installation script to install all dependencies:
   ```sh
   scripts\install_dependencies.bat
   ```
3. Run the project using the local run script:
   ```sh
   scripts\run_local.bat
   ```

### On Scala

To run the Scala connector locally, follow these steps:

1. Ensure Scala and sbt are installed on your system.
2. Navigate to the project directory.
3. Compile the Scala code using sbt:
   ```sh
   sbt compile
   ```
4. Run the Scala connector:
   ```sh
   sbt run
   ```

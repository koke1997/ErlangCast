# ScalaCast
ScalaCast is a centralized server streaming video server built with Scala, leveraging its strengths in concurrency and distributed programming. Features include centralized server, video chunking, reliable data transmission, and fault-tolerant architecture. Ideal for real-time streaming and centralized applications.

## Centralized Server
The centralized server feature allows clients to connect to a central server for efficient communication and data sharing.

### How to Use Centralized Server
1. Start the centralized server by calling the `CentralizedServer.startServer` function.
2. Handle client requests using the `CentralizedServer.handleClientRequest` function.
3. Stop the centralized server by calling the `CentralizedServer.stopServer` function.

### Example
```scala
import scala.concurrent.Await
import scala.concurrent.duration._

implicit val ec: ExecutionContext = ExecutionContext.global

// Start the centralized server
Await.result(CentralizedServer.startServer(8080), 5.seconds)

// Handle a client request
Await.result(CentralizedServer.handleClientRequest(clientSocket), 5.seconds)

// Stop the centralized server
Await.result(CentralizedServer.stopServer(), 5.seconds)
```

## Video Chunking
The video chunking feature allows the system to divide video files into smaller chunks for transmission, enabling efficient streaming and data handling.

### How to Use Video Chunking
1. Chunk a video file by calling the `VideoChunking.chunkVideo` function with the video file path and chunk size as arguments.
2. Retrieve a specific chunk of a video file using the `VideoChunking.getChunk` function with the video file path and chunk index as arguments.

### Example
```scala
import scala.util.{Success, Failure}

// Chunk a video file into 1MB chunks
VideoChunking.chunkVideo("path/to/video.mp4", 1048576) match {
  case Success(chunks) => println(s"Video chunked into ${chunks.length} parts")
  case Failure(exception) => println(s"Failed to chunk video: ${exception.getMessage}")
}

// Retrieve the first chunk of the video file
VideoChunking.getChunk("path/to/video.mp4", 1) match {
  case Success(chunk) => println(s"Retrieved chunk of size ${chunk.length}")
  case Failure(exception) => println(s"Failed to retrieve chunk: ${exception.getMessage}")
}
```

## Reliable Data Transmission
The reliable data transmission feature ensures that data is transmitted reliably between clients and the server, handling packet loss and retransmissions.

### How to Use Reliable Data Transmission
1. Send data reliably by calling the `ReliableTransmission.sendData` function with the client and data as arguments.
2. Receive data reliably by calling the `ReliableTransmission.receiveData` function with a data handler function as an argument.

### Example
```scala
import scala.concurrent.Await
import scala.concurrent.duration._

implicit val ec: ExecutionContext = ExecutionContext.global

// Send data reliably to a client
Await.result(ReliableTransmission.sendData("client1", "Hello, client!"), 5.seconds)

// Define a data handler function
val dataHandler: String => Unit = data => println(s"Received data: $data")

// Receive data reliably
Await.result(ReliableTransmission.receiveData(dataHandler), 5.seconds)
```

## Fault-Tolerant Architecture
The fault-tolerant architecture ensures that the system can handle failures gracefully and continue operating. This includes error handling and recovery mechanisms in various modules.

### Fault-Tolerant Features
1. **Centralized Server**: The `CentralizedServer` module includes error handling and recovery mechanisms for server failures.
2. **Reliable Transmission**: The `ReliableTransmission` module includes error handling for retry limit reached and logs the error.
3. **Video Chunking**: The `VideoChunking` module includes error handling for file operations and chunking process.
4. **System Configuration**: The `sys.config` file includes fault-tolerance configurations for the system.

### Example
```scala
import scala.concurrent.Await
import scala.concurrent.duration._

implicit val ec: ExecutionContext = ExecutionContext.global

// Example of error handling in centralized server
Await.result(CentralizedServer.startServer(8080), 5.seconds) match {
  case Success(_) => println("Centralized server started successfully")
  case Failure(exception) => println(s"Failed to start centralized server: ${exception.getMessage}")
}

// Example of error handling in reliable transmission
Await.result(ReliableTransmission.sendData("client1", "Hello, client!"), 5.seconds) match {
  case Success(_) => println("Data sent successfully")
  case Failure(exception) => println(s"Failed to send data: ${exception.getMessage}")
}

// Example of error handling in video chunking
VideoChunking.chunkVideo("path/to/video.mp4", 1048576) match {
  case Success(chunks) => println("Video chunked successfully")
  case Failure(exception) => println(s"Failed to chunk video: ${exception.getMessage}")
}
```

## Scala Connector
The Scala connector enables communication between the Scala server and the Scala videoserver, leveraging Akka for actor-based communication. Scala is also used to raise the needed frontend to interact with the videoserver.

### How to Use Scala Connector
1. Set up Scala and necessary dependencies.
2. Use the `ScalaConnector` object to send and receive messages between Scala and the videoserver.

### Example
```scala
import ScalaConnector._

object Main extends App {
  // Send a message to the videoserver
  sendToErlang("Hello, videoserver!")

  // Receive a message from the videoserver
  val message = Await.result(receiveFromErlang(), 5.seconds)
  println(s"Received message from videoserver: $message")
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
sbt test
```

### Running Integration Tests
To run the integration tests, use the following command:
```sh
sbt it:test
```

### Running Test Coverage
To run the test coverage, use the following command:
```sh
sbt coverage test
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

      - name: Set up Java
        uses: actions/setup-java@v2
        with:
          distribution: 'adopt'
          java-version: '11'

      - name: Set up Scala
        run: |
          echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
          echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
          curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
          sudo apt-get update
          sudo apt-get install sbt

      - name: Compile Scala code
        run: sbt compile

      - name: Run unit tests
        run: sbt test

      - name: Run integration tests
        run: sbt it:test
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

## Running the Scala Frontend Locally

### On Linux

To run the Scala frontend locally on Linux, follow these steps:

1. Open a terminal and navigate to the project directory.
2. Ensure Scala and sbt are installed on your system.
3. Add the sbt repository to your sources list:
   ```sh
   echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
   echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
   curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
   sudo apt-get update
   sudo apt-get install sbt
   ```
4. Compile the Scala code using sbt:
   ```sh
   sbt compile
   ```
5. Run the Scala frontend:
   ```sh
   sbt run
   ```

### On Windows

To run the Scala frontend locally on Windows, follow these steps:

1. Open a command prompt and navigate to the project directory.
2. Ensure Scala and sbt are installed on your system.
3. Add the sbt repository to your sources list:
   ```sh
   echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
   echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
   curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
   sudo apt-get update
   sudo apt-get install sbt
   ```
4. Compile the Scala code using sbt:
   ```sh
   sbt compile
   ```
5. Run the Scala frontend:
   ```sh
   sbt run
   ```

## HLS and DASH Streaming Options
The HLS and DASH streaming options allow for adaptive bitrate streaming, providing a better user experience by adjusting the video quality based on the viewer's network conditions.

### How to Use HLS Streaming
1. Ensure the HLS library is installed and configured in your project.
2. Use the `hls_streaming:start/2` function to start HLS streaming with the video file path and output directory as arguments.
3. Retrieve the HLS playlist using the `hls_streaming:get_playlist/1` function with the output directory as an argument.

### Example
```scala
// Start HLS streaming
HlsStreaming.start("path/to/video.mp4", "path/to/output")

// Retrieve the HLS playlist
val playlist = HlsStreaming.getPlaylist("path/to/output")
println(s"HLS playlist: $playlist")
```

### How to Use DASH Streaming
1. Ensure the DASH library is installed and configured in your project.
2. Use the `dash_streaming:start/2` function to start DASH streaming with the video file path and output directory as arguments.
3. Retrieve the DASH manifest using the `dash_streaming:get_manifest/1` function with the output directory as an argument.

### Example
```scala
// Start DASH streaming
DashStreaming.start("path/to/video.mp4", "path/to/output")

// Retrieve the DASH manifest
val manifest = DashStreaming.getManifest("path/to/output")
println(s"DASH manifest: $manifest")
```

## Deploying the App to GitHub Pages

To deploy the app to GitHub Pages, follow these steps:

1. Ensure that the GitHub repository is set up with GitHub Pages enabled.
2. Add the GitHub Actions workflow file (`.github/workflows/deploy.yml`) to automate the deployment process.
3. Push the changes to the `main` branch.

The GitHub Actions workflow will automatically build and deploy the app to GitHub Pages.

### Example Workflow File
The following is an example of a GitHub Actions workflow file (`.github/workflows/deploy.yml`) that automates the deployment to GitHub Pages:
```yaml
name: Deploy to GitHub Pages

on:
  push:
    branches:
      - main

jobs:
  build:
    runs-on: ubuntu-latest

    steps:
      - name: Checkout code
        uses: actions/checkout@v2

      - name: Set up Java
        uses: actions/setup-java@v2
        with:
          distribution: 'adopt'
          java-version: '11'

      - name: Set up Scala
        run: |
          echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
          echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
          curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
          sudo apt-get update
          sudo apt-get install sbt

      - name: Compile Scala code
        run: sbt compile

      - name: Build frontend
        run: sbt fullOptJS

      - name: Deploy to GitHub Pages
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./docs
```

## RTMP Streaming Option
The RTMP streaming option allows for real-time messaging protocol streaming, providing a stable video session for multiple users.

### How to Use RTMP Streaming
1. Ensure the `nginx-rtmp-module` and `ffmpeg` with RTMP support are installed and configured in your project.
2. Use the `rtmp_streaming:start/2` function to start RTMP streaming with the video file path and output directory as arguments.
3. Retrieve the RTMP stream URL using the `rtmp_streaming:get_stream_url/1` function with the output directory as an argument.

### Example
```scala
// Start RTMP streaming
RtmpStreaming.start("path/to/video.mp4", "path/to/output")

// Retrieve the RTMP stream URL
val streamUrl = RtmpStreaming.getStreamUrl("path/to/output")
println(s"RTMP stream URL: $streamUrl")
```

## Troubleshooting Common Issues
This section provides solutions to common issues that users might encounter during setup and running the project.

### Issue: Dependencies not installed correctly
**Solution**: Ensure that the installation scripts (`scripts/install_dependencies.sh` and `scripts/install_dependencies.bat`) are up-to-date and cover all necessary dependencies for the project, including those required for RTMP streaming.

### Issue: Video streaming not working
**Solution**: Check the configuration files and ensure that the necessary libraries and tools for HLS, DASH, and RTMP streaming are installed and configured correctly. Verify that the video file paths and output directories are correct.

### Issue: Project not running locally
**Solution**: Follow the detailed instructions in the `README.md` for setting up and running the project locally on different platforms. Ensure that all dependencies are installed, and the project is compiled correctly.

## Running the Project with Docker

To build and run the project using Docker, follow these steps:

1. Build the Docker image:
   ```sh
   docker build -t scalacast .
   ```

2. Run the Docker container:
   ```sh
   docker run -p 8080:8080 -p 9100-9155:9100-9155 -p 9200-9255:9200-9255 scalacast
   ```

This will start the ScalaCast project in a Docker container, making it easy to run locally.

## Microservices Architecture

The ScalaCast project now includes a microservices architecture for easier integration and stability. The microservices are designed to handle specific functionalities and can be run independently.

### Microservices Overview

1. **Centralized Server Service**: Handles client connections and requests.
2. **Video Chunking Service**: Manages video chunking for efficient streaming.
3. **Reliable Transmission Service**: Ensures reliable data transmission between clients and the server.
4. **Fault Tolerance Service**: Provides fault tolerance and error recovery mechanisms.

### Setting Up and Running Microservices

To set up and run the microservices, follow these steps:

1. Ensure Docker is installed on your system.
2. Navigate to the project directory.
3. Build the Docker images for each microservice:
   ```sh
   docker build -t centralized-server-service -f Dockerfile .
   docker build -t video-chunking-service -f Dockerfile .
   docker build -t reliable-transmission-service -f Dockerfile .
   docker build -t fault-tolerance-service -f Dockerfile .
   ```
4. Run the Docker containers for each microservice:
   ```sh
   docker run -d --name centralized-server-service centralized-server-service
   docker run -d --name video-chunking-service video-chunking-service
   docker run -d --name reliable-transmission-service reliable-transmission-service
   docker run -d --name fault-tolerance-service fault-tolerance-service
   ```

### Using Microservices

#### Centralized Server Service

The Centralized Server Service handles client connections and requests.

**Functions**:
- `startServer`: Starts the centralized server.
- `stopServer`: Stops the centralized server.
- `handleClientRequest`: Handles client requests.

#### Video Chunking Service

The Video Chunking Service manages video chunking for efficient streaming.

**Functions**:
- `chunkVideo`: Chunks a video file into smaller chunks.
- `getChunk`: Retrieves a specific chunk of a video file.

#### Reliable Transmission Service

The Reliable Transmission Service ensures reliable data transmission between clients and the server.

**Functions**:
- `sendData`: Sends data to a client with retry mechanism.
- `receiveData`: Receives data and processes it using the provided data handler.

#### Fault Tolerance Service

The Fault Tolerance Service provides fault tolerance and error recovery mechanisms.

**Functions**:
- `start`: Starts the fault tolerance service.
- `stop`: Stops the fault tolerance service.
- `handleError`: Handles errors and logs them.
- `recover`: Recovers from errors based on the error type.

## Running the App on Multiple Ports

To run the app on multiple ports, follow these steps:

1. Update the `config/sys.config` file to include configurations for two different ports.
2. Modify the `Dockerfile` to expose the new ports and run the app on both ports.
3. Update the `docs/index.html` file to allow selecting a camera input for each port.
4. Use the provided Python script to run the app on multiple ports using parameters.

### Example Configuration

#### `config/sys.config`
```scala
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
  {centralized_server, [
    {port_range, {9200, 9255}},
    {broadcast_interval, 5000},
    {max_clients, 50}
  ]},
  {fault_tolerance, [
    {retry_limit, 5},
    {ack_timeout, 1000},
    {max_retries, 3}
  ]},
  {http, [
    {port1, 8080},
    {port2, 8081}
  ]}
]
```

#### `Dockerfile`
```Dockerfile
# Build stage for Scala
FROM ubuntu:24.04 AS scala-build
WORKDIR /app

# Install Java and SBT dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        openjdk-11-jdk-headless \
        curl \
        gnupg \
    && echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list \
    && curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | gpg --dearmor > /etc/apt/trusted.gpg.d/sbt.gpg \
    && apt-get update \
    && apt-get install -y sbt \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Setup SBT
RUN mkdir -p project && \
    echo 'sbt.version=1.8.2' > project/build.properties && \
    echo 'addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.16")\naddSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.1.1")' > project/plugins.sbt && \
    echo 'name := "scalacast"\nversion := "0.1.0"\nscalaVersion := "2.13.8"' > build.sbt

COPY src/scala ./src/scala/
RUN sbt compile

# Final runtime stage
FROM ubuntu:24.04
WORKDIR /app

ENV NODE_NAME_1=node1@127.0.0.1 \
    NODE_NAME_2=node2@127.0.0.1 \
    COOKIE=scalacast_cookie \
    DEBIAN_FRONTEND=noninteractive

# Install runtime dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        openjdk-11-jre-headless \
        netcat-openbsd \
        curl \
    && apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Setup directories
RUN mkdir -p /app/log /app/media/input /app/media/output && \
    chmod -R 777 /app/log /app/media

# Copy artifacts and configs
COPY --from=scala-build /app/target /app/target
COPY config ./config/

# Create startup script
RUN echo '#!/bin/sh\n\
epmd -daemon\n\
NODE_NAME=$NODE_NAME_1 COOKIE=$COOKIE sbt run & \
NODE_NAME=$NODE_NAME_2 COOKIE=$COOKIE sbt run\n' > /usr/local/bin/start.sh && \
    chmod +x /usr/local/bin/start.sh

EXPOSE 8080 8081 9100-9155 9200-9255

HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD nc -z localhost 8080 || exit 1

CMD ["/usr/local/bin/start.sh"]
```

#### `docs/index.html`
```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Webcam Test - Scala Frontend</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            text-align: center;
            margin: 0;
            padding: 0;
        }
        video {
            width: 80%;
            max-width: 600px;
            margin: 20px auto;
            display: block;
        }
        select {
            margin: 20px;
            padding: 10px;
            font-size: 16px;
        }
    </style>
</head>
<body>
    <h1>Webcam Test</h1>
    <select id="streamingOptions">
        <option value="webcam">Webcam</option>
        <option value="hls">HLS</option>
        <option value="dash">DASH</option>
        <option value="rtmp">RTMP</option>
    </select>
    <select id="user1Camera">
        <option value="">Select Camera for User 1</option>
    </select>
    <select id="user2Camera">
        <option value="">Select Camera for User 2</option>
    </select>
    <select id="port1Camera">
        <option value="">Select Camera for Port 1</option>
    </select>
    <select id="port2Camera">
        <option value="">Select Camera for Port 2</option>
    </select>
    <video id="videoElement" autoplay controls></video>
    <script>
        const videoElement = document.getElementById('videoElement');
        const streamingOptions = document.getElementById('streamingOptions');
        const user1Camera = document.getElementById('user1Camera');
        const user2Camera = document.getElementById('user2Camera');
        const port1Camera = document.getElementById('port1Camera');
        const port2Camera = document.getElementById('port2Camera');

        function getCameras() {
            return navigator.mediaDevices.enumerateDevices()
                .then(devices => devices.filter(device => device.kind === 'videoinput'));
        }

        function populateCameraOptions(selectElement, cameras) {
            cameras.forEach(camera => {
                const option = document.createElement('option');
                option.value = camera.deviceId;
                option.text = camera.label || `Camera ${selectElement.length + 1}`;
                selectElement.appendChild(option);
            });
        }

        getCameras().then(cameras => {
            populateCameraOptions(user1Camera, cameras);
            populateCameraOptions(user2Camera, cameras);
            populateCameraOptions(port1Camera, cameras);
            populateCameraOptions(port2Camera, cameras);
        });

        streamingOptions.addEventListener('change', function() {
            const selectedOption = streamingOptions.value;
            if (selectedOption === 'webcam') {
                if (navigator.mediaDevices && navigator.mediaDevices.getUserMedia) {
                    navigator.mediaDevices.getUserMedia({ video: true })
                        .then(function(stream) {
                            videoElement.srcObject = stream;
                            videoElement.play();
                        })
                        .catch(function(error) {
                            console.error("Error accessing webcam: ", error);
                        });
                } else {
                    console.error("getUserMedia not supported by this browser.");
                }
            } else if (selectedOption === 'hls') {
                videoElement.src = 'path/to/hls/playlist.m3u8';
                videoElement.play();
            } else if (selectedOption === 'dash') {
                videoElement.src = 'path/to/dash/manifest.mpd';
                videoElement.play();
            } else if (selectedOption === 'rtmp') {
                videoElement.src = 'rtmp://localhost/live/stream';
                videoElement.play();
            }
        });

        user1Camera.addEventListener('change', function() {
            const selectedCameraId = user1Camera.value;
            if (selectedCameraId) {
                navigator.mediaDevices.getUserMedia({ video: { deviceId: { exact: selectedCameraId } } })
                    .then(function(stream) {
                        videoElement.srcObject = stream;
                        videoElement.play();
                    })
                    .catch(function(error) {
                        console.error("Error accessing camera for User 1: ", error);
                    });
            }
        });

        user2Camera.addEventListener('change', function() {
            const selectedCameraId = user2Camera.value;
            if (selectedCameraId) {
                navigator.mediaDevices.getUserMedia({ video: { deviceId: { exact: selectedCameraId } } })
                    .then(function(stream) {
                        videoElement.srcObject = stream;
                        videoElement.play();
                    })
                    .catch(function(error) {
                        console.error("Error accessing camera for User 2: ", error);
                    });
            }
        });

        port1Camera.addEventListener('change', function() {
            const selectedCameraId = port1Camera.value;
            if (selectedCameraId) {
                navigator.mediaDevices.getUserMedia({ video: { deviceId: { exact: selectedCameraId } } })
                    .then(function(stream) {
                        videoElement.srcObject = stream;
                        videoElement.play();
                    })
                    .catch(function(error) {
                        console.error("Error accessing camera for Port 1: ", error);
                    });
            }
        });

        port2Camera.addEventListener('change', function() {
            const selectedCameraId = port2Camera.value;
            if (selectedCameraId) {
                navigator.mediaDevices.getUserMedia({ video: { deviceId: { exact: selectedCameraId } } })
                    .then(function(stream) {
                        videoElement.srcObject = stream;
                        videoElement.play();
                    })
                    .catch(function(error) {
                        console.error("Error accessing camera for Port 2: ", error);
                    });
            }
        });

        // Initialize with webcam option
        streamingOptions.value = 'webcam';
        streamingOptions.dispatchEvent(new Event('change'));
    </script>
    <!-- Scala is used for the frontend -->
</body>
</html>
```

#### `scripts/run_local.py`
```python
import subprocess
import argparse

def run_app(port1, port2, camera1, camera2):
    cmd = f"sbt run & sbt run --setcookie port2 --name port2@127.0.0.1"
    subprocess.run(cmd, shell=True)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run the app on multiple ports with camera inputs")
    parser.add_argument("--port1", type=int, default=8080, help="Port for the first instance")
    parser.add_argument("--port2", type=int, default=8081, help="Port for the second instance")
    parser.add_argument("--camera1", type=str, help="Camera input for the first instance")
    parser.add_argument("--camera2", type=str, help="Camera input for the second instance")
    args = parser.parse_args()

    run_app(args.port1, args.port2, args.camera1, args.camera2)
```

## Running the Project Locally

To run the project locally, follow these steps:

1. Open a terminal or command prompt and navigate to the project directory.
2. Run the installation script to install all dependencies:
   ```sh
   ./scripts/install_dependencies.sh
   ```
3. Run the project using the Python script:
   ```sh
   python scripts/run_local.py
   ```

### Note
The `scripts/run_local.sh` and `scripts/run_local.bat` scripts are deprecated. Please use the `scripts/run_local.py` script for local testing and running the project.

## Webcam Test Site

The project is now ready for release as a site for testing webcam functionality. The webcam test site allows users to select different streaming protocols and camera inputs for testing purposes.

### Features
- Select different streaming protocols: Webcam, HLS, DASH, RTMP
- Choose camera inputs for different users and ports
- Display the selected camera input in a video element

### How to Use the Webcam Test Site
1. Open the `docs/index.html` file in a web browser.
2. Select a streaming protocol from the dropdown menu.
3. Choose a camera input for each user and port.
4. The selected camera input will be displayed in the video element.

### Example
```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Webcam Test - ScalaCast</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            text-align: center;
            margin: 0;
            padding: 0;
        }
        video {
            width: 80%;
            max-width: 600px;
            margin: 20px auto;
            display: block;
        }
        select {
            margin: 20px;
            padding: 10px;
            font-size: 16px;
        }
    </style>
</head>
<body>
    <h1>Webcam Test</h1>
    <select id="streamingOptions">
        <option value="webcam">Webcam</option>
        <option value="hls">HLS</option>
        <option value="dash">DASH</option>
        <option value="rtmp">RTMP</option>
    </select>
    <select id="user1Camera">
        <option value="">Select Camera for User 1</option>
    </select>
    <select id="user2Camera">
        <option value="">Select Camera for User 2</option>
    </select>
    <select id="port1Camera">
        <option value="">Select Camera for Port 1</option>
    </select>
    <select id="port2Camera">
        <option value="">Select Camera for Port 2</option>
    </select>
    <div id="selectedCameraInput">
        <h2>Selected Camera Input</h2>
        <video id="selectedCameraVideo" autoplay controls></video>
    </div>
    <video id="videoElement" autoplay controls></video>
    <script>
        const videoElement = document.getElementById('videoElement');
        const streamingOptions = document.getElementById('streamingOptions');
        const user1Camera = document.getElementById('user1Camera');
        const user2Camera = document.getElementById('user2Camera');
        const port1Camera = document.getElementById('port1Camera');
        const port2Camera = document.getElementById('port2Camera');
        const selectedCameraVideo = document.getElementById('selectedCameraVideo');

        function getCameras() {
            return navigator.mediaDevices.enumerateDevices()
                .then(devices => devices.filter(device => device.kind === 'videoinput'));
        }

        function populateCameraOptions(selectElement, cameras) {
            cameras.forEach(camera => {
                const option = document.createElement('option');
                option.value = camera.deviceId;
                option.text = camera.label || `Camera ${selectElement.length + 1}`;
                selectElement.appendChild(option);
            });
        }

        getCameras().then(cameras => {
            populateCameraOptions(user1Camera, cameras);
            populateCameraOptions(user2Camera, cameras);
            populateCameraOptions(port1Camera, cameras);
            populateCameraOptions(port2Camera, cameras);
        });

        streamingOptions.addEventListener('change', function() {
            const selectedOption = streamingOptions.value;
            if (selectedOption === 'webcam') {
                if (navigator.mediaDevices && navigator.mediaDevices.getUserMedia) {
                    navigator.mediaDevices.getUserMedia({ video: true })
                        .then(function(stream) {
                            videoElement.srcObject = stream;
                            videoElement.play();
                        })
                        .catch(function(error) {
                            console.error("Error accessing webcam: ", error);
                        });
                } else {
                    console.error("getUserMedia not supported by this browser.");
                }
            } else if (selectedOption === 'hls') {
                videoElement.src = 'path/to/hls/playlist.m3u8';
                videoElement.play();
            } else if (selectedOption === 'dash') {
                videoElement.src = 'path/to/dash/manifest.mpd';
                videoElement.play();
            } else if (selectedOption === 'rtmp') {
                videoElement.src = 'rtmp://localhost/live/stream';
                videoElement.play();
            }
        });

        user1Camera.addEventListener('change', function() {
            const selectedCameraId = user1Camera.value;
            if (selectedCameraId) {
                navigator.mediaDevices.getUserMedia({ video: { deviceId: { exact: selectedCameraId } } })
                    .then(function(stream) {
                        videoElement.srcObject = stream;
                        videoElement.play();
                        selectedCameraVideo.srcObject = stream;
                        selectedCameraVideo.play();
                    })
                    .catch(function(error) {
                        console.error("Error accessing camera for User 1: ", error);
                    });
            }
        });

        user2Camera.addEventListener('change', function() {
            const selectedCameraId = user2Camera.value;
            if (selectedCameraId) {
                navigator.mediaDevices.getUserMedia({ video: { deviceId: { exact: selectedCameraId } } })
                    .then(function(stream) {
                        videoElement.srcObject = stream;
                        videoElement.play();
                        selectedCameraVideo.srcObject = stream;
                        selectedCameraVideo.play();
                    })
                    .catch(function(error) {
                        console.error("Error accessing camera for User 2: ", error);
                    });
            }
        });

        port1Camera.addEventListener('change', function() {
            const selectedCameraId = port1Camera.value;
            if (selectedCameraId) {
                navigator.mediaDevices.getUserMedia({ video: { deviceId: { exact: selectedCameraId } } })
                    .then(function(stream) {
                        videoElement.srcObject = stream;
                        videoElement.play();
                        selectedCameraVideo.srcObject = stream;
                        selectedCameraVideo.play();
                    })
                    .catch(function(error) {
                        console.error("Error accessing camera for Port 1: ", error);
                    });
            }
        });

        port2Camera.addEventListener('change', function() {
            const selectedCameraId = port2Camera.value;
            if (selectedCameraId) {
                navigator.mediaDevices.getUserMedia({ video: { deviceId: { exact: selectedCameraId } } })
                    .then(function(stream) {
                        videoElement.srcObject = stream;
                        videoElement.play();
                        selectedCameraVideo.srcObject = stream;
                        selectedCameraVideo.play();
                    })
                    .catch(function(error) {
                        console.error("Error accessing camera for Port 2: ", error);
                    });
            }
        });

        // Initialize with webcam option
        streamingOptions.value = 'webcam';
        streamingOptions.dispatchEvent(new Event('change'));
    </script>
    <!-- Scala is used for the frontend -->
</body>
</html>
```

# Use an official Erlang runtime as a parent image
FROM erlang:24

# Set the working directory
WORKDIR /app

# Copy the current directory contents into the container at /app
COPY . /app

# Install dependencies
RUN apt-get update && \
    apt-get install -y ffmpeg libav-tools libavcodec-extra && \
    rebar3 get-deps

# Compile the project
RUN rebar3 compile

# Install Scala and sbt
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list && \
    echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list && \
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add && \
    apt-get update && \
    apt-get install -y sbt

# Compile the Scala code
RUN sbt compile

# Copy and run the peer discovery microservice
COPY microservices/peer_discovery_service.erl /app/microservices/
RUN erlc /app/microservices/peer_discovery_service.erl
CMD ["erl", "-noshell", "-s", "peer_discovery_service", "start", "-s", "init", "stop"]

# Copy and run the video chunking microservice
COPY microservices/video_chunking_service.erl /app/microservices/
RUN erlc /app/microservices/video_chunking_service.erl
CMD ["erl", "-noshell", "-s", "video_chunking_service", "chunk_video", "-s", "init", "stop"]

# Copy and run the reliable transmission microservice
COPY microservices/reliable_transmission_service.erl /app/microservices/
RUN erlc /app/microservices/reliable_transmission_service.erl
CMD ["erl", "-noshell", "-s", "reliable_transmission_service", "send_data", "-s", "init", "stop"]

# Copy and run the fault tolerance microservice
COPY microservices/fault_tolerance_service.erl /app/microservices/
RUN erlc /app/microservices/fault_tolerance_service.erl
CMD ["erl", "-noshell", "-s", "fault_tolerance_service", "start", "-s", "init", "stop"]

# Expose the necessary ports
EXPOSE 8080 9100-9155 9200-9255

# Define the command to run the project
CMD ["rebar3", "shell"]

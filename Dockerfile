# Use multi-stage builds to optimize the build process

# First stage: Build Erlang project
FROM erlang:24 AS erlang-build

# Set the working directory
WORKDIR /app

# Copy the current directory contents into the container at /app
COPY . /app

#Install dependencies
RUN apt-get update && \
    apt-get install -y ffmpeg libavcodec-extra && \
    rebar3 get-deps

# Compile the Erlang project
RUN rebar3 compile

# Second stage: Build Scala project
FROM openjdk:11 AS scala-build

# Set the working directory
WORKDIR /app

# Copy the current directory contents into the container at /app
COPY . /app

# Install Scala and sbt
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list && \
    echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list && \
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add && \
    apt-get update && \
    apt-get install -y sbt

# Compile the Scala code
RUN sbt compile

# Final stage: Set up runtime environment
FROM erlang:24

# Set the working directory
WORKDIR /app

# Copy the compiled artifacts from the previous stages
COPY --from=erlang-build /app /app
COPY --from=scala-build /app /app

# Expose the necessary ports
EXPOSE 8080 8081 9100-9155 9200-9255

# Define the command to run the project on both ports
CMD ["sh", "-c", "rebar3 shell & rebar3 shell --setcookie port2 --name port2@127.0.0.1"]

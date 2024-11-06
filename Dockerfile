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

# Expose the necessary ports
EXPOSE 8080 9100-9155 9200-9255

# Define the command to run the project
CMD ["rebar3", "shell"]

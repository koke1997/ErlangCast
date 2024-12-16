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
    echo 'name := "erlangcast"\nversion := "0.1.0"\nscalaVersion := "2.13.8"' > build.sbt

COPY src/scala ./src/scala/
RUN sbt compile

# Final runtime stage
FROM ubuntu:24.04
WORKDIR /app

ENV NODE_NAME_1=node1@127.0.0.1 \
    NODE_NAME_2=node2@127.0.0.1 \
    COOKIE=erlangcast_cookie \
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

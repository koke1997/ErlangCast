#!/bin/bash

# Update package list
sudo apt-get update

# Install Erlang and rebar3
sudo apt-get install -y erlang rebar3

# Install ffmpeg
sudo apt-get install -y ffmpeg

# Install HLS and DASH dependencies
sudo apt-get install -y libav-tools
sudo apt-get install -y libavcodec-extra

# Print success message
echo "All dependencies have been successfully installed."

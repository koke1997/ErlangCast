#!/bin/bash

# Update package list
sudo apt-get update

# Install Erlang and rebar3
sudo apt-get install -y erlang rebar3

# Install ffmpeg
sudo apt-get install -y ffmpeg

# Print success message
echo "All dependencies have been successfully installed."

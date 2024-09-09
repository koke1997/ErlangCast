#!/bin/bash

# Run the project locally on Linux

# Call the script to install dependencies
./scripts/install_dependencies.sh

# Use rebar3 to compile the project
rebar3 compile

# Use rebar3 to run the project
rebar3 shell

# Print success message after running
echo "The project is now running locally."

#!/bin/bash

# Deployment script for ErlangCast

# Function to log messages
log_message() {
    echo "$(date +'%Y-%m-%d %H:%M:%S') - $1"
}

# Function to handle errors
handle_error() {
    log_message "ERROR: $1"
    exit 1
}

# Build the project using rebar3
log_message "Building the project..."
rebar3 compile || handle_error "Failed to build the project"

# Start the server
log_message "Starting the server..."
erl -pa _build/default/lib/*/ebin -sname erlangcast -s erlangcast start || handle_error "Failed to start the server"

# Deploy the demo page to GitHub Pages
log_message "Deploying the demo page to GitHub Pages..."
git config --global user.name 'github-actions'
git config --global user.email 'github-actions@github.com'
git add docs/index.html
git commit -m "Deploy demo page"
git push origin main || handle_error "Failed to deploy the demo page"

log_message "Deployment completed successfully"

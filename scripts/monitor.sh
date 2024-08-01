#!/bin/bash

# Monitoring script for ErlangCast

# Function to log messages
log_message() {
    echo "$(date +'%Y-%m-%d %H:%M:%S') - $1"
}

# Function to handle errors
handle_error() {
    log_message "ERROR: $1"
    exit 1
}

# Function to monitor CPU usage
monitor_cpu() {
    log_message "Monitoring CPU usage..."
    top -b -n1 | grep "Cpu(s)" | awk '{print $2 + $4}' || handle_error "Failed to monitor CPU usage"
}

# Function to monitor memory usage
monitor_memory() {
    log_message "Monitoring memory usage..."
    free -m | awk 'NR==2{printf "Memory Usage: %s/%sMB (%.2f%%)\n", $3,$2,$3*100/$2 }' || handle_error "Failed to monitor memory usage"
}

# Function to monitor logs for errors
monitor_logs() {
    log_message "Monitoring logs for errors..."
    tail -f log/error.log | grep --line-buffered "ERROR" || handle_error "Failed to monitor logs for errors"
}

# Main monitoring function
main() {
    log_message "Starting monitoring..."
    monitor_cpu &
    monitor_memory &
    monitor_logs &
    wait
}

main

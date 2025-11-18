#!/bin/bash
#
# Recording Station - Run Script
# This script sets up the library path and runs the application
#

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Set library path to current directory
export LD_LIBRARY_PATH="${SCRIPT_DIR}:${LD_LIBRARY_PATH}"

# Run the application
exec "${SCRIPT_DIR}/onairchive" "$@"
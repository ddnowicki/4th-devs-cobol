#!/bin/bash
# Auto-restart wrapper for COBOL HTTP server
# The COBOL server sometimes crashes after handling nginx-proxied requests
# This keeps it running automatically

trap '' PIPE

while true; do
    echo "[wrapper] Starting COBOL HTTP server..."
    ./app
    EXIT_CODE=$?
    echo "[wrapper] Server exited with code $EXIT_CODE, restarting in 0.5s..."
    sleep 0.5
done

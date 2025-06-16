#!/bin/bash
set -e

echo "=== APRS Server Startup Debug Script ==="
echo "Time: $(date)"
echo "Working directory: $(pwd)"
echo "User: $(whoami)"
echo "UID: $(id -u)"

echo ""
echo "=== Environment Variables ==="
echo "PORT: ${PORT:-'not set'}"
echo "PHX_SERVER: ${PHX_SERVER:-'not set'}"
echo "PHX_HOST: ${PHX_HOST:-'not set'}"
echo "MIX_ENV: ${MIX_ENV:-'not set'}"
echo "DATABASE_URL: ${DATABASE_URL:+***set***}"
echo "SECRET_KEY_BASE: ${SECRET_KEY_BASE:+***set***}"

echo ""
echo "=== File System Check ==="
echo "App directory contents:"
ls -la /app/
echo ""
echo "Bin directory contents:"
ls -la /app/bin/
echo ""
echo "Server script permissions:"
ls -la /app/bin/server

echo ""
echo "=== Network Configuration ==="
echo "Available network interfaces:"
ip addr show 2>/dev/null || ifconfig 2>/dev/null || echo "Network tools not available"

echo ""
echo "=== Starting Phoenix Server ==="
echo "Executing: /app/bin/server"
echo "Expected to bind to port: ${PORT:-4000}"
echo ""

# Start the server
exec /app/bin/server

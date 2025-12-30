#!/bin/bash
set -e

echo "Setting up APRS.me development environment..."

# Fix permissions on mounted volumes
echo "Fixing permissions on mounted volumes..."
sudo chown -R vscode:vscode /workspace/deps /workspace/_build 2>/dev/null || true
sudo chmod -R 755 /workspace/deps /workspace/_build 2>/dev/null || true

# Install dependencies
echo "Installing Elixir dependencies..."
mix deps.get

# Wait for PostgreSQL to be ready
echo "Waiting for PostgreSQL..."
until pg_isready -h localhost -U postgres; do
  echo "PostgreSQL is unavailable - sleeping"
  sleep 2
done

echo "PostgreSQL is ready!"

# Setup database
echo "Setting up database..."
mix ecto.setup || mix ecto.create

# Run migrations
echo "Running migrations..."
mix ecto.migrate

echo "Development environment setup complete!"
echo "You can start the Phoenix server with: mix phx.server"
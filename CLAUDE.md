# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Elixir Phoenix LiveView application that serves as a real-time APRS (Automatic Packet Reporting System) tracker and visualizer. It connects to the APRS-IS network to receive live amateur radio packets and displays them on an interactive map interface.

## Development Commands

### Setup
- `mix setup` - Complete project setup (deps.get + ecto.setup)
- `mix deps.get` - Install dependencies
- `mix ecto.setup` - Create database, run migrations, and seed data
- `mix ecto.reset` - Drop and recreate database
- `mix phx.server` - Start Phoenix server (http://localhost:4000)
- `iex -S mix phx.server` - Start server in interactive Elixir shell

### Testing
- `mix test` - Run full test suite
- `mix test --stale` - Run only tests affected by code changes
- `mix test.watch` - Continuous testing with file watching
- `mix test --cover` - Generate test coverage reports

### Code Quality
- `mix format` - Format code according to .formatter.exs
- `mix credo` - Static code analysis and style checking
- `mix dialyzer` - Static type analysis (must run and fix errors/warnings)
- `mix sobelow` - Security vulnerability scanning

### Assets (No Node.js)
- `mix assets.deploy` - Build and minify frontend assets (Tailwind CSS + ESBuild)
- Phoenix uses ESBuild directly without Node.js package managers

## Architecture

### Core Components
- **Aprsme.AprsIsConnection** - TCP connection to APRS-IS network with reconnection logic
- **Aprsme.PacketConsumer** - Processes incoming APRS packets using GenStage pipeline
- **Aprsme.Packet** - Database schema for APRS packets with PostGIS geographic data
- **AprsmeWeb.MapLive.Index** - Main real-time map interface using Phoenix LiveView
- **Aprsme.Workers.PacketCleanupWorker** - Oban background job for data cleanup

### Data Flow
1. APRS-IS connection receives packets via TCP
2. PacketConsumer processes packets through GenStage pipeline
3. Packets stored in PostgreSQL with PostGIS geographic indexing
4. LiveView broadcasts real-time updates to connected clients via PubSub
5. Background workers handle cleanup and maintenance tasks

### Key Dependencies
- Phoenix LiveView for real-time UI without JavaScript
- PostGIS for geographic data storage and spatial queries
- Oban for background job processing
- GenStage for packet processing pipelines
- Tailwind CSS + ESBuild for frontend assets (no Node.js)

## Testing Patterns

Tests use comprehensive mocking to prevent external connections:
- APRS-IS connections are mocked in test environment
- Database uses sandbox mode for isolation
- External API calls mocked with Mox library
- Run `mix test` after any changes to ensure no breakage

## Code Style Guidelines

- Use LiveView for UI interactions, minimize JavaScript
- Prefer pattern matching over if/case statements
- Follow idiomatic Elixir conventions
- Run `mix format` before committing
- Address any compiler warnings
- Run `mix dialyzer` and fix all errors/warnings
- Use function composition over nested conditionals

## Deployment

The application supports Kubernetes deployment with manifests in `k8s/` directory and GitHub Actions CI/CD pipeline. Database migrations run automatically via init containers.
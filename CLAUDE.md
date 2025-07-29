# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Prerequisites

Before setting up the project, ensure you have the following installed:
- Elixir 1.17+
- Erlang/OTP
- PostgreSQL with PostGIS extension

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
- **CRITICAL**: ALWAYS run `mix format` BEFORE committing - never commit unformatted code
- **MANDATORY**: Run `mix compile --warnings-as-errors` and ensure it passes before considering any task complete

### Assets (No Node.js Required)
- `mix assets.deploy` - Build and minify frontend assets (Tailwind CSS + ESBuild)
- Phoenix uses standalone ESBuild and Tailwind binaries - no npm/yarn needed
- JavaScript bundling handled by ESBuild
- CSS compilation handled by Tailwind CLI

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

## Test-Driven Development

**MANDATORY**: Follow strict test-driven development (TDD) practices:

1. **Red Phase**: Write failing tests first before implementing any functionality
2. **Green Phase**: Write minimal code to make tests pass
3. **Refactor Phase**: Improve code while keeping tests green

### TDD Workflow
- Always write tests before implementing new features or fixing bugs
- Start with the simplest failing test case
- Write only enough code to make the test pass
- Refactor with confidence knowing tests will catch regressions
- Run `mix test` frequently during development
- Use `mix test.watch` for continuous feedback

### Testing Patterns

Tests use comprehensive mocking to prevent external connections:
- APRS-IS connections are mocked in test environment
- Database uses sandbox mode for isolation
- External API calls mocked with Mox library
- Write unit tests for business logic, integration tests for workflows
- Test edge cases and error conditions thoroughly
- Maintain high test coverage with `mix test --cover`

## Code Style Guidelines

- **CRITICAL**: Never write production code without tests first
- Use LiveView for UI interactions, minimize JavaScript
- Prefer pattern matching over if/case statements
- Follow idiomatic Elixir conventions
- **CRITICAL**: ALWAYS run `mix format` BEFORE committing - this is non-negotiable
- Address any compiler warnings
- Run `mix dialyzer` and fix all errors/warnings
- **MANDATORY**: Run `mix compile --warnings-as-errors` and ensure it passes before considering any task complete
- Use function composition over nested conditionals
- Write descriptive test names that explain behavior

### Pre-Commit Checklist
**CRITICAL**: NEVER commit or push code with syntax errors or compilation failures. Always validate before committing.

**MANDATORY PRE-COMMIT STEPS** - Must be executed in this exact order before EVERY git commit:

1. **`mix format`** - ALWAYS run this FIRST before any git operations
   - This formats all Elixir code according to project standards
   - **NEVER skip this step** - unformatted code should never be committed
   
2. **`mix compile --warnings-as-errors`** - ensure no warnings or compilation errors
   - Must pass with zero warnings and zero errors
   
3. **`mix test`** - ensure all tests pass (at minimum, ensure no syntax/compilation errors)
   - At minimum verify no compilation failures even if some tests fail
   
4. **MANDATORY**: If any step fails, fix the issues before proceeding
   
5. Only after ALL checks pass should you commit and push your changes

**NEVER PUSH BROKEN CODE**: Syntax errors, compilation failures, or basic test failures should be fixed immediately before any git operations. Pushing broken code breaks CI/CD pipelines and wastes deployment resources.

**REMEMBER**: Always run `mix format` before every commit - this is non-negotiable and must become automatic habit.

## Important Documentation Updates

- **MANDATORY**: Whenever you implement improvements or changes to the system:
  1. Update `/CHANGELOG.md` with:
     - Add new entries under `[Unreleased]` section
     - Use categories: Added, Changed, Fixed, Removed
     - Be specific and user-focused in descriptions
  2. Update `/docs/improvement-todos.md` with:
     - Mark completed items as done with the implementation date
     - Add any new improvements discovered during implementation
  - Update priority levels based on new insights
  - Document any technical decisions or trade-offs made
- This ensures continuity across sessions and helps track progress on system improvements

## Web Testing

- **MANDATORY**: When viewing any website or web application, always use Puppeteer to take screenshots and interact with the page
- Use `mcp__puppeteer__puppeteer_navigate`, `mcp__puppeteer__puppeteer_screenshot`, and other Puppeteer tools
- This ensures accurate visual feedback and proper testing of the user interface

## Deployment

The application supports Kubernetes deployment with manifests in `k8s/` directory and GitHub Actions CI/CD pipeline. Database migrations run automatically via init containers.

### Infrastructure

The application runs on a highly available infrastructure:
- **Kubernetes**: k3s cluster deployed across 3 VMs
- **Virtualization**: Proxmox VE hosting the VMs
- **Hardware**: 3 Intel N100 nodes, each with:
  - 32GB RAM
  - 1TB SSD storage
  - Low power consumption (~15W per node)
- **Distribution**: VMs spread across physical nodes for hardware redundancy

### Kubernetes Commands

The app is deployed in a k3s cluster with the following structure:
- **App name**: `aprs`
- **Namespace**: `aprs`
- **Deployment**: StatefulSet with 2-3 replicas
- **Manifests**: Located in `~/dev/infra/clusters/aprs/`

Common kubectl commands for debugging:
```bash
# Check pod status
kubectl get pods -n aprs

# Get logs from the app
kubectl logs -f deployment/aprs -n aprs

# Get logs from a specific pod
kubectl logs <pod-name> -n aprs

# Describe pod for events and details
kubectl describe pod <pod-name> -n aprs

# Restart the statefulset
kubectl rollout restart statefulset/aprs -n aprs

# Check statefulset status
kubectl rollout status statefulset/aprs -n aprs

# Execute commands in the pod (StatefulSet)
kubectl exec -it aprs-0 -n aprs -- /app/bin/aprsme remote

# Check cluster membership
kubectl exec -it <pod-name> -n aprs -- /app/bin/aprsme eval "Node.list()"

# Check leader status
kubectl exec -it <pod-name> -n aprs -- /app/bin/aprsme eval "Aprsme.Cluster.LeaderElection.is_leader?()"
```

### Clustering Architecture

The application uses distributed Erlang clustering to ensure only one APRS-IS connection across multiple replicas:

1. **StatefulSet Deployment**: 
   - Uses Kubernetes StatefulSet for stable pod names (aprs-0, aprs-1, etc.)
   - Headless service provides DNS entries for each pod
   - Stable network identities enable Erlang distribution

2. **Leader Election**: Uses `:global` registry for distributed leader election
   - Only the elected leader maintains the APRS-IS connection
   - Automatic failover when leader goes down
   - Leader election managed by `Aprsme.Cluster.LeaderElection`

3. **Connection Management**: 
   - `Aprsme.Cluster.ConnectionManager` starts/stops APRS-IS based on leadership
   - Uses `DynamicSupervisor` to manage connection lifecycle
   - Prevents duplicate connections and packet processing

4. **Cluster Configuration**:
   - Uses `libcluster` with Kubernetes.DNS strategy
   - Automatic node discovery via headless service
   - Erlang cookie configured via RELEASE_COOKIE environment variable
   - Environment variables:
     - `CLUSTER_ENABLED=true` - Enables clustering
     - `RELEASE_NODE` - Erlang node name
     - `RELEASE_COOKIE` - Erlang distribution cookie

4. **Deployment**:
   - Default replicas: 3 (configurable in `aprs-deployment.yaml`)
   - Only leader processes APRS packets
   - All nodes serve web traffic

## 1Password Integration

When working with passwords and secrets:
- **ALWAYS** use 1Password account ID: `YOOATCZZSVGH7AD6VABUVPORLI`
- Store all passwords, API keys, and secrets in 1Password
- Use the `op` CLI tool for programmatic access
- Never hardcode passwords in configuration files
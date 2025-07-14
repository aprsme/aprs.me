# Technology Stack

## Core Technologies

- **Elixir**: ~> 1.17 - Primary programming language
- **Phoenix Framework**: ~> 1.8 - Web framework
- **Phoenix LiveView**: ~> 1.0.17 - Real-time web interfaces
- **PostgreSQL**: Database with PostGIS extension for geospatial data
- **Ecto**: Database wrapper and query generator

## Key Dependencies

### Web & UI

- **Bandit**: HTTP server (replaces Cowboy)
- **Phoenix LiveDashboard**: Development and monitoring dashboard
- **Tailwind CSS**: Utility-first CSS framework
- **Heroicons**: Icon library
- **ESBuild**: JavaScript bundler

### Data Processing

- **GenStage**: Stream processing for APRS packet pipeline
- **Oban**: Background job processing
- **Cachex**: In-memory caching
- **Geo/PostGIS**: Geospatial data handling

### External Integrations

- **HTTPoison/Req**: HTTP clients
- **Jason**: JSON encoding/decoding
- **Swoosh**: Email delivery

### Development Tools

- **Credo**: Static code analysis
- **Dialyxir**: Static type analysis
- **Styler**: Code formatting
- **ExVCR**: HTTP interaction recording for tests
- **Sobelow**: Security-focused static analysis

## Common Commands

### Development Setup

```bash
mix deps.get              # Install dependencies
mix ecto.setup            # Create and migrate database
mix phx.server            # Start development server
iex -S mix phx.server     # Start with interactive shell
```

### Database Operations

```bash
mix ecto.create           # Create database
mix ecto.migrate          # Run migrations
mix ecto.reset            # Drop, create, and migrate database
mix ecto.gen.migration    # Generate new migration
```

### Code Quality

```bash
mix credo                 # Run static analysis
mix dialyzer              # Run type analysis
mix format                # Format code
mix test                  # Run test suite
mix test.watch            # Run tests in watch mode
```

### Asset Management

```bash
mix assets.deploy         # Build and optimize assets for production
mix tailwind default      # Compile Tailwind CSS
mix esbuild default       # Compile JavaScript
```

### Production

```bash
mix release               # Build production release
mix phx.digest            # Generate asset digests
```

## Configuration

- **Development**: `config/dev.exs`
- **Production**: `config/prod.exs`
- **Runtime**: `config/runtime.exs`
- **Test**: `config/test.exs`
- **Base**: `config/config.exs`

## Code Style

- Uses **Styler** plugin for consistent formatting
- **Credo** enforces code quality with 120 character line limit
- **Dialyzer** for static type analysis
- Import dependencies: `:ecto`, `:ecto_sql`, `:phoenix`, `:stream_data`

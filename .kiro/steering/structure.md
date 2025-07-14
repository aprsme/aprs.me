# Project Structure

## Root Directory Layout

```
├── lib/                    # Application source code
├── test/                   # Test files
├── config/                 # Configuration files
├── priv/                   # Private application files (migrations, static assets)
├── assets/                 # Frontend assets (CSS, JS, images)
├── deps/                   # Dependencies (managed by Mix)
├── _build/                 # Compiled artifacts
├── k8s/                    # Kubernetes deployment manifests
├── rel/                    # Release configuration
└── vendor/                 # Vendored dependencies (custom APRS library)
```

## Application Code Structure (`lib/`)

### Core Application (`lib/aprsme/`)

- **Business Logic**: Core domain modules for APRS packet processing
- **Data Layer**: Ecto schemas, repos, and database interactions
- **Background Jobs**: Oban workers for maintenance tasks
- **External Integrations**: APRS-IS connection and packet processing pipeline

#### Key Subdirectories:

- `accounts/` - User authentication and management
- `is/` - APRS-IS server connection and supervision
- `packets/` - Packet processing, clustering, and queries
- `workers/` - Background job workers

### Web Layer (`lib/aprsme_web/`)

- **Controllers**: HTTP request handlers and API endpoints
- **LiveViews**: Real-time interactive pages
- **Components**: Reusable UI components
- **Plugs**: Custom middleware for authentication, rate limiting, etc.

#### Key Subdirectories:

- `controllers/` - Traditional Phoenix controllers and API endpoints
- `live/` - Phoenix LiveView modules organized by feature
- `components/` - Shared UI components and layouts
- `plugs/` - Custom Plug modules

## Configuration Structure (`config/`)

- `config.exs` - Base configuration
- `dev.exs` - Development environment
- `prod.exs` - Production environment
- `runtime.exs` - Runtime configuration (environment variables)
- `test.exs` - Test environment

## Test Structure (`test/`)

- Mirrors `lib/` structure with `_test.exs` suffix
- `test/support/` - Test helpers and fixtures
- `test/fixtures/` - Test data fixtures
- `test/integration/` - Integration tests

## Asset Structure (`assets/`)

- `css/` - Tailwind CSS files
- `js/` - JavaScript/TypeScript files
  - `features/` - Feature-specific JS modules
  - `hooks/` - Phoenix LiveView hooks
  - `types/` - TypeScript type definitions

## Database Structure (`priv/repo/`)

- `migrations/` - Ecto database migrations
- `seeds.exs` - Database seeding script

## Naming Conventions

### Modules

- **Contexts**: `Aprsme.ContextName` (e.g., `Aprsme.Accounts`, `Aprsme.Packets`)
- **Schemas**: `Aprsme.Context.SchemaName` (e.g., `Aprsme.Accounts.User`)
- **Web Modules**: `AprsmeWeb.ModuleName` (e.g., `AprsmeWeb.UserController`)
- **LiveViews**: `AprsmeWeb.FeatureLive.Action` (e.g., `AprsmeWeb.MapLive.Index`)

### Files

- **Contexts**: `snake_case.ex` (e.g., `packet_consumer.ex`)
- **Tests**: `module_name_test.exs`
- **Templates**: `action_name.html.heex` for LiveView templates

## Architecture Patterns

### Phoenix Contexts

- Business logic organized into bounded contexts
- Each context has a main module that serves as the public API
- Internal modules handle specific responsibilities

### GenStage Pipeline

- `PacketProducer` → `PacketConsumer` pipeline for APRS data processing
- Supervised by `PacketPipelineSupervisor`
- Configurable batch processing parameters

### LiveView Organization

- Feature-based organization (e.g., `map_live/`, `packets_live/`)
- Shared components in `components/`
- Shared utilities in `shared/`

### Error Handling

- `ErrorTracker` for application error monitoring
- Custom error handlers and circuit breakers for external services
- Structured logging with sanitization for sensitive data

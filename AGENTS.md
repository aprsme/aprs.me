# Repository Guidelines

## Project Structure & Module Organization
Elixir application code lives in `lib/`: `lib/aprsme` holds domain contexts, `lib/aprsme_web` contains LiveView, controllers, and templates, and `lib/mix` exposes project-specific Mix tasks. Front-end assets are in `assets/` (split into `js/`, `css/`, and component bundles under `assets/`). Database artifacts sit in `priv/repo/` with migrations and seeds, while `config/` provides environment-specific settings. Tests reside in `test/` with shared helpers in `test/support/`. Supporting tooling (Docker, scripts, CI manifests) lives under `scripts/`, `rel/`, and `k8s/`.

## Build, Test, and Development Commands
Run `mix setup` once to fetch dependencies and prepare the Postgres database. Use `mix phx.server` (or `iex -S mix phx.server`) for local development. Execute `mix test` for the full ExUnit suite; `mix test.watch` keeps rerunning on file changes. For quality gates, `mix credo --strict`, `mix dialyzer`, and `mix sobelow` mirror CI checks. Before releases or deployments, build static assets with `MIX_ENV=prod mix assets.deploy` and create a release via `mix release`.

## Coding Style & Naming Conventions
Follow idiomatic Elixir with 2-space indentation and modules in `PascalCase`. The top-level `.formatter.exs` enforces a 120-character line limit, imports Phoenix HTML formatting, and loads the Styler plugin—always run `mix format` before committing. Keep function names in `snake_case`, LiveView hooks under `assets/js` in `camelCase`, and CSS utilities organized by Tailwind conventions. Prefer contexts for new domain modules and colocate schemas with their contexts inside `lib/aprsme`.

## Testing Guidelines
The project uses ExUnit with database sandboxing; aliases ensure `ecto.create` and `ecto.migrate` run before tests. Property-based tests live alongside unit tests using `StreamData`, and browser flows use Wallaby helpers in `test/support`. Name test files `<feature>_test.exs` and ensure async-friendly tests opt in with `use ExUnit.Case, async: true`. Check coverage locally with `mix coveralls.html` and review the generated report in `cover/`.

## Commit & Pull Request Guidelines
Commit history favors short, imperative subjects with optional Conventional Commit prefixes (`fix:`, `refactor:`). Keep each commit focused, formatted, and passing the suite. PRs should outline the change, reference related issues, note DB or config impacts, and include screenshots or logs for UI-facing updates. Confirm that formatting, tests, and static analysis commands above complete successfully before requesting review.

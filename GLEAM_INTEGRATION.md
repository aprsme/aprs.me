# Gleam Integration Guide

This document describes how Gleam has been integrated into the APRS.me Elixir project.

## Setup

1. **Mix Gleam Archive**: Installed via `mix archive.install hex mix_gleam`
2. **Dependencies**: Added to mix.exs:
   ```elixir
   {:gleam_stdlib, ">= 0.60.0 and < 1.0.0", app: false, override: true},
   {:gleeunit, "~> 1.0", only: [:dev, :test], runtime: false, app: false}
   ```
3. **Project Configuration**: Added to mix.exs project config:
   ```elixir
   archives: [mix_gleam: "~> 0.6"],
   erlc_paths: ["build/dev/erlang/aprsme/_gleam_artefacts", "src"],
   erlc_include_path: "build/dev/erlang/aprsme/include",
   ```

## File Structure

- `/src/` - Gleam source files
- `/src/aprs/` - APRS-specific Gleam modules
- `/gleam.toml` - Gleam project configuration

## Compilation

The project is configured to automatically compile Gleam code when running tests or compiling:

```bash
# For development
mix compile.gleam && mix compile

# For tests (automatically compiles Gleam)
mix test

# Manual compilation if needed
mix gleam_compile
```

The custom `gleam_compile` task handles:
- Running the mix_gleam compiler when available
- Falling back to the `gleam` binary if mix_gleam isn't installed
- Copying compiled beam files to the appropriate build directory

## Module Naming

Gleam modules are compiled with `@` as the separator in BEAM files:
- Gleam: `aprs/encoding`
- BEAM: `aprs@encoding`
- Elixir: `:aprs@encoding`

## Current Modules

### encoding.gleam

A type-safe implementation of encoding utilities:
- `sanitize_string/1` - Ensures strings are valid UTF-8, handles Latin-1 conversion
- `to_float_safe/1` - Safe string to float conversion with Option type  
- `to_hex/1` - Convert binary to hex string representation
- `has_weather_data/4` - Check if packet contains weather data
- `encoding_info/1` - Get encoding information about a binary

## Elixir Integration

The `Aprsme.EncodingUtils` module now wraps the Gleam implementation, replacing the original pure Elixir version. The Gleam implementation provides:
- Type-safe string sanitization with Latin-1 to UTF-8 conversion
- Proper handling of control characters
- Safe float conversion with bounds checking  
- Consistent encoding validation

The migration was completed with all tests passing and no breaking changes to the API.

## Testing

The original test suite at `/test/aprsme/encoding_utils_test.exs` continues to work with the Gleam implementation:
```bash
mix test test/aprsme/encoding_utils_test.exs
```

## Future Considerations

1. Add Gleam compiler to Mix.compilers() once the integration is more stable
2. Consider migrating more type-critical modules to Gleam
3. Explore using Gleam's type system for packet validation
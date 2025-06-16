# Test Environment APRS Isolation

This directory contains support files to ensure that the APRS.me application does not make external network connections during testing.

## Overview

The APRS.me application normally connects to external APRS-IS (Automatic Packet Reporting System - Internet Service) servers to receive real-time amateur radio packet data. During testing, we need to prevent these external connections to ensure:

1. **Test Isolation**: Tests run independently without external dependencies
2. **Network Security**: No unintended external connections during CI/CD
3. **Performance**: Tests run faster without network delays
4. **Reliability**: Tests don't fail due to external service availability

## Implementation

### Configuration-Based Prevention

The primary mechanism is environment-based configuration in `config/test.exs`:

```elixir
# Disable APRS-IS external connections in test environment
config :aprs,
  aprs_is_server: nil,
  aprs_is_port: nil,
  aprs_is_default_filter: nil,
  aprs_is_login_id: nil,
  aprs_is_password: nil,
  disable_aprs_connection: true
```

### Application-Level Guards

The `Aprs.Application` module only starts the APRS-IS supervisor in `:prod` and `:dev` environments:

```elixir
children =
  if Application.get_env(:aprs, :env) in [:prod, :dev] do
    children ++ [Aprs.Is.IsSupervisor]
  else
    children
  end
```

### Module-Level Safeguards

The `Aprs.Is` module includes additional safeguards to prevent accidental connections:

- Early termination if started in test environment
- Connection blocking in the `connect_to_aprs_is/2` function
- Safe fallback responses for status queries

### Mock Implementation

The `AprsIsMock` module (`aprs_is_mock.ex`) provides a test-safe implementation that:

- Mimics the interface of the real `Aprs.Is` module
- Returns realistic status information without external connections
- Allows simulation of packet reception for testing
- Provides connection state simulation for comprehensive testing

## Usage in Tests

### Basic Setup

The mock is automatically configured in `test_helper.exs`:

```elixir
# Ensure no external APRS connections during tests
Application.put_env(:aprs, :disable_aprs_connection, true)
Code.require_file("support/aprs_is_mock.ex", __DIR__)
```

### Using the Mock

```elixir
# Start the mock in your test setup
{:ok, _pid} = AprsIsMock.start_link()

# Get status (no external connection)
status = AprsIsMock.get_status()

# Simulate packet reception
test_packet = %{sender: "TEST-1", latitude: 33.0, longitude: -96.0}
AprsIsMock.simulate_packet(test_packet)

# Simulate connection state changes
AprsIsMock.simulate_connection_state(true)  # connected
AprsIsMock.simulate_connection_state(false) # disconnected
```

### Testing Network Isolation

The `Aprs.IsTest` module includes comprehensive tests to verify:

- APRS.Is module doesn't start in test environment
- Configuration properly disables connections
- No real APRS servers are contacted
- Mock provides expected functionality

## Verification

To verify that external connections are properly blocked:

1. **Run the test suite**: `mix test test/aprs/is_test.exs`
2. **Check configuration**: Ensure test config sets `disable_aprs_connection: true`
3. **Monitor network**: During test runs, no connections should be made to APRS-IS servers
4. **Review logs**: Test environment should log connection prevention messages

## Security Notes

- Never commit real APRS credentials to test configurations
- Use placeholder values like "TEST" for callsigns in tests
- Ensure production credentials are only available in production environment
- Regularly audit test configurations to prevent credential leakage

## Common APRS-IS Servers (Blocked in Tests)

The following servers should never be contacted during testing:

- `rotate.aprs2.net` (Primary rotation server)
- `dallas.aprs2.net` (Dallas server)
- `seattle.aprs2.net` (Seattle server)
- `chicago.aprs2.net` (Chicago server)
- `atlanta.aprs2.net` (Atlanta server)

Any test configuration pointing to these servers indicates a misconfiguration.

## Troubleshooting

### Tests Hanging or Timing Out

If tests hang, check:
- APRS.Is process isn't starting (`Process.whereis(Aprs.Is)` should return `nil`)
- Test configuration has `disable_aprs_connection: true`
- No real server addresses in test config

### External Connection Errors in Tests

If you see TCP connection errors during tests:
- Verify the application supervision tree excludes APRS-IS in test
- Check that `Mix.env()` returns `:test`
- Ensure test_helper.exs properly configures the environment

### Mock Not Working

If the mock doesn't provide expected responses:
- Verify `AprsIsMock` is properly loaded in test_helper.exs
- Check that the mock is started in test setup
- Ensure mock methods match the real module's interface

## Contributing

When adding new APRS-related functionality:

1. **Update the mock** to include new methods
2. **Add test coverage** for network isolation
3. **Verify configuration** prevents external connections
4. **Document any new** test isolation requirements
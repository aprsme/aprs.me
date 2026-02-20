defmodule Aprsme.CircuitBreakerTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias Aprsme.CircuitBreaker

  # Use unique service names per test to avoid cross-test interference
  # since CircuitBreaker is already started by the application.
  # CircuitBreaker.call/3 uses Task.async internally, so errors in the
  # called function produce EXIT signals. We trap exits in the test process.

  setup do
    Process.flag(:trap_exit, true)
    :ok
  end

  describe "state transitions" do
    test "starts in closed state for unknown service" do
      assert CircuitBreaker.get_state(:cb_test_closed) == :closed
    end

    test "transitions to open after reaching failure threshold" do
      capture_log(fn ->
        for _ <- 1..5 do
          CircuitBreaker.call(:cb_test_open, fn -> raise "fail" end)
        end
      end)

      assert CircuitBreaker.get_state(:cb_test_open) == :open
    end

    test "stays closed when below failure threshold" do
      capture_log(fn ->
        for _ <- 1..4 do
          CircuitBreaker.call(:cb_test_below, fn -> raise "fail" end)
        end
      end)

      assert CircuitBreaker.get_state(:cb_test_below) == :closed
    end

    test "resets failure count on success" do
      capture_log(fn ->
        for _ <- 1..3 do
          CircuitBreaker.call(:cb_test_reset_count, fn -> raise "fail" end)
        end
      end)

      CircuitBreaker.call(:cb_test_reset_count, fn -> :ok end)

      stats = CircuitBreaker.get_stats(:cb_test_reset_count)
      assert stats.failure_count == 0
      assert stats.state == :closed
    end
  end

  describe "half-open state persistence" do
    test "half_open state is persisted after recovery timeout elapses" do
      # Trip the breaker
      capture_log(fn ->
        for _ <- 1..5 do
          CircuitBreaker.call(:cb_test_persist_ho, fn -> raise "fail" end)
        end
      end)

      assert CircuitBreaker.get_state(:cb_test_persist_ho) == :open

      # Manipulate last_failure_time to simulate recovery_timeout having passed
      :sys.replace_state(CircuitBreaker, fn services ->
        service_state = Map.get(services, :cb_test_persist_ho)

        updated =
          Map.put(
            service_state,
            :last_failure_time,
            DateTime.add(DateTime.utc_now(), -60, :second)
          )

        Map.put(services, :cb_test_persist_ho, updated)
      end)

      # First call to get_state should return half_open AND persist it
      assert CircuitBreaker.get_state(:cb_test_persist_ho) == :half_open

      # The persisted state should now be half_open
      stats = CircuitBreaker.get_stats(:cb_test_persist_ho)
      assert stats.state == :half_open
    end

    test "second get_state call returns half_open from persisted state" do
      # Trip the breaker
      capture_log(fn ->
        for _ <- 1..5 do
          CircuitBreaker.call(:cb_test_second_ho, fn -> raise "fail" end)
        end
      end)

      # Set last_failure_time to the past
      :sys.replace_state(CircuitBreaker, fn services ->
        service_state = Map.get(services, :cb_test_second_ho)

        updated =
          Map.put(
            service_state,
            :last_failure_time,
            DateTime.add(DateTime.utc_now(), -60, :second)
          )

        Map.put(services, :cb_test_second_ho, updated)
      end)

      # First call transitions to half_open and persists
      assert CircuitBreaker.get_state(:cb_test_second_ho) == :half_open

      # Second call should also return half_open (from persisted state, not recalculated)
      assert CircuitBreaker.get_state(:cb_test_second_ho) == :half_open

      stats = CircuitBreaker.get_stats(:cb_test_second_ho)
      assert stats.state == :half_open
    end
  end

  describe "call/3" do
    test "returns {:ok, result} on success" do
      assert {:ok, 42} = CircuitBreaker.call(:cb_test_call_ok, fn -> 42 end)
    end

    test "returns {:error, :circuit_open} when circuit is open" do
      capture_log(fn ->
        for _ <- 1..5 do
          CircuitBreaker.call(:cb_test_call_open, fn -> raise "fail" end)
        end
      end)

      capture_log(fn ->
        assert {:error, :circuit_open} = CircuitBreaker.call(:cb_test_call_open, fn -> :ok end)
      end)
    end

    test "allows calls in half_open state and closes on success" do
      # Trip the breaker
      capture_log(fn ->
        for _ <- 1..5 do
          CircuitBreaker.call(:cb_test_ho_call, fn -> raise "fail" end)
        end
      end)

      # Fast-forward past recovery timeout
      :sys.replace_state(CircuitBreaker, fn services ->
        service_state = Map.get(services, :cb_test_ho_call)

        updated =
          Map.put(
            service_state,
            :last_failure_time,
            DateTime.add(DateTime.utc_now(), -60, :second)
          )

        Map.put(services, :cb_test_ho_call, updated)
      end)

      # Should be half_open and allow the call
      assert {:ok, :recovered} = CircuitBreaker.call(:cb_test_ho_call, fn -> :recovered end)

      # Should be back to closed after success
      assert CircuitBreaker.get_state(:cb_test_ho_call) == :closed
    end
  end

  describe "reset/1" do
    test "resets circuit to closed state" do
      capture_log(fn ->
        for _ <- 1..5 do
          CircuitBreaker.call(:cb_test_reset, fn -> raise "fail" end)
        end
      end)

      assert CircuitBreaker.get_state(:cb_test_reset) == :open

      capture_log(fn ->
        CircuitBreaker.reset(:cb_test_reset)
      end)

      assert CircuitBreaker.get_state(:cb_test_reset) == :closed
      stats = CircuitBreaker.get_stats(:cb_test_reset)
      assert stats.failure_count == 0
    end
  end
end

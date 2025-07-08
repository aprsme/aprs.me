defmodule Aprsme.CircuitBreaker do
  @moduledoc """
  Circuit breaker implementation for external service calls to prevent cascading failures
  """
  use GenServer

  alias Aprsme.LogSanitizer

  require Logger

  @type state :: :closed | :open | :half_open
  @type service_state :: %{
          state: state(),
          failure_count: non_neg_integer(),
          failure_threshold: non_neg_integer(),
          timeout: non_neg_integer(),
          last_failure_time: DateTime.t() | nil,
          recovery_timeout: non_neg_integer()
        }

  @default_failure_threshold 5
  @default_timeout 5000
  @default_recovery_timeout 30_000

  # Public API

  @doc """
  Start the circuit breaker GenServer
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Execute a function with circuit breaker protection
  """
  @spec call(atom(), function(), non_neg_integer()) :: {:ok, any()} | {:error, :circuit_open | :timeout | any()}
  def call(service_name, fun, timeout \\ @default_timeout) when is_function(fun, 0) do
    case get_state(service_name) do
      :open ->
        Logger.warning("Circuit breaker open for service",
          circuit_breaker:
            LogSanitizer.log_data(
              service: service_name,
              state: :open,
              action: :rejected
            )
        )

        {:error, :circuit_open}

      state when state in [:closed, :half_open] ->
        attempt_call(service_name, fun, timeout)
    end
  end

  @doc """
  Get the current state of a service circuit breaker
  """
  @spec get_state(atom()) :: state()
  def get_state(service_name) do
    GenServer.call(__MODULE__, {:get_state, service_name})
  end

  @doc """
  Get statistics for a service circuit breaker
  """
  @spec get_stats(atom()) :: service_state()
  def get_stats(service_name) do
    GenServer.call(__MODULE__, {:get_stats, service_name})
  end

  @doc """
  Manually reset a circuit breaker
  """
  @spec reset(atom()) :: :ok
  def reset(service_name) do
    GenServer.call(__MODULE__, {:reset, service_name})
  end

  # GenServer implementation

  @impl true
  def init(_opts) do
    {:ok, %{}}
  end

  @impl true
  def handle_call({:get_state, service_name}, _from, services) do
    service_state = get_service_state(services, service_name)
    current_state = calculate_current_state(service_state)
    {:reply, current_state, services}
  end

  @impl true
  def handle_call({:get_stats, service_name}, _from, services) do
    service_state = get_service_state(services, service_name)
    {:reply, service_state, services}
  end

  @impl true
  def handle_call({:reset, service_name}, _from, services) do
    default_state = default_service_state()
    new_services = Map.put(services, service_name, default_state)

    Logger.info("Circuit breaker reset",
      circuit_breaker:
        LogSanitizer.log_data(
          service: service_name,
          action: :manual_reset
        )
    )

    {:reply, :ok, new_services}
  end

  @impl true
  def handle_call({:record_success, service_name}, _from, services) do
    service_state = get_service_state(services, service_name)
    updated_state = %{service_state | failure_count: 0, state: :closed}
    new_services = Map.put(services, service_name, updated_state)
    {:reply, :ok, new_services}
  end

  @impl true
  def handle_call({:record_failure, service_name}, _from, services) do
    service_state = get_service_state(services, service_name)
    new_failure_count = service_state.failure_count + 1

    new_state =
      if new_failure_count >= service_state.failure_threshold do
        :open
      else
        service_state.state
      end

    updated_state = %{
      service_state
      | failure_count: new_failure_count,
        state: new_state,
        last_failure_time: DateTime.utc_now()
    }

    if new_state == :open do
      Logger.warning("Circuit breaker opened due to failure threshold",
        circuit_breaker:
          LogSanitizer.log_data(
            service: service_name,
            failure_count: new_failure_count,
            threshold: service_state.failure_threshold,
            state: :open
          )
      )
    end

    new_services = Map.put(services, service_name, updated_state)
    {:reply, :ok, new_services}
  end

  # Private functions

  defp attempt_call(service_name, fun, timeout) do
    task = Task.async(fn -> fun.() end)

    try do
      result = Task.await(task, timeout)
      record_success(service_name)
      {:ok, result}
    catch
      :exit, {:timeout, _} ->
        Task.shutdown(task, :brutal_kill)
        record_failure(service_name)

        Logger.warning("Service call timed out",
          circuit_breaker:
            LogSanitizer.log_data(
              service: service_name,
              timeout_ms: timeout,
              action: :timeout_failure
            )
        )

        {:error, :timeout}

      kind, error ->
        record_failure(service_name)

        Logger.error("Service call failed",
          circuit_breaker:
            LogSanitizer.log_data(
              service: service_name,
              error_kind: kind,
              error: LogSanitizer.sanitize(error)
            )
        )

        {:error, error}
    end
  end

  defp record_success(service_name) do
    GenServer.call(__MODULE__, {:record_success, service_name})
  end

  defp record_failure(service_name) do
    GenServer.call(__MODULE__, {:record_failure, service_name})
  end

  defp get_service_state(services, service_name) do
    Map.get(services, service_name, default_service_state())
  end

  defp default_service_state do
    %{
      state: :closed,
      failure_count: 0,
      failure_threshold: @default_failure_threshold,
      timeout: @default_timeout,
      last_failure_time: nil,
      recovery_timeout: @default_recovery_timeout
    }
  end

  defp calculate_current_state(%{state: :open, last_failure_time: last_failure_time, recovery_timeout: recovery_timeout}) do
    if last_failure_time && DateTime.diff(DateTime.utc_now(), last_failure_time, :millisecond) >= recovery_timeout do
      :half_open
    else
      :open
    end
  end

  defp calculate_current_state(%{state: state}), do: state
end

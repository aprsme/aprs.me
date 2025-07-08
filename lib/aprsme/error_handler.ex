defmodule Aprsme.ErrorHandler do
  @moduledoc """
  Centralized error handling and recovery patterns for the application
  """

  alias Aprsme.LogSanitizer
  alias Ecto.Query.CastError

  require Logger

  @doc """
  Handle and log database errors with context
  """
  def handle_database_error(error, context \\ %{}) do
    sanitized_context = LogSanitizer.sanitize_error_context(context)

    case error do
      %CastError{} = cast_error ->
        Logger.error("Database cast error - invalid data type",
          error_details:
            LogSanitizer.log_data(
              error_type: :cast_error,
              message: cast_error.message,
              context: sanitized_context
            )
        )

        {:error, :invalid_data}

      %Ecto.ConstraintError{} = constraint_error ->
        Logger.error("Database constraint violation",
          error_details:
            LogSanitizer.log_data(
              error_type: :constraint_violation,
              constraint: constraint_error.constraint,
              context: sanitized_context
            )
        )

        {:error, :constraint_violation}

      %Postgrex.Error{postgres: %{code: :unique_violation}} ->
        Logger.warning("Attempted to insert duplicate record",
          error_details:
            LogSanitizer.log_data(
              error_type: :duplicate_record,
              context: sanitized_context
            )
        )

        {:error, :duplicate_record}

      %DBConnection.ConnectionError{} ->
        Logger.error("Database connection error",
          error_details:
            LogSanitizer.log_data(
              error_type: :connection_error,
              context: sanitized_context
            )
        )

        {:error, :database_unavailable}

      %Ecto.QueryError{} = query_error ->
        Logger.error("Database query error",
          error_details:
            LogSanitizer.log_data(
              error_type: :query_error,
              message: query_error.message,
              context: sanitized_context
            )
        )

        {:error, :query_failed}

      error ->
        Logger.error("Unexpected database error",
          error_details:
            LogSanitizer.log_data(
              error_type: :unknown_database_error,
              error: LogSanitizer.sanitize(error),
              context: sanitized_context
            )
        )

        {:error, :database_error}
    end
  end

  @doc """
  Handle network and external service errors
  """
  def handle_network_error(error, service_name, context \\ %{}) do
    sanitized_context = LogSanitizer.sanitize_error_context(context)

    case error do
      %Req.TransportError{reason: :timeout} ->
        Logger.warning("Network request timeout",
          network_error:
            LogSanitizer.log_data(
              service: service_name,
              error_type: :timeout,
              context: sanitized_context
            )
        )

        {:error, :timeout}

      %Req.TransportError{reason: :econnrefused} ->
        Logger.error("Connection refused by service",
          network_error:
            LogSanitizer.log_data(
              service: service_name,
              error_type: :connection_refused,
              context: sanitized_context
            )
        )

        {:error, :service_unavailable}

      %Req.TransportError{reason: reason} ->
        Logger.error("Network transport error",
          network_error:
            LogSanitizer.log_data(
              service: service_name,
              error_type: :transport_error,
              reason: reason,
              context: sanitized_context
            )
        )

        {:error, :network_error}

      {:error, :circuit_open} ->
        Logger.warning("Circuit breaker open for service",
          network_error:
            LogSanitizer.log_data(
              service: service_name,
              error_type: :circuit_open,
              context: sanitized_context
            )
        )

        {:error, :service_unavailable}

      error ->
        Logger.error("Unexpected network error",
          network_error:
            LogSanitizer.log_data(
              service: service_name,
              error_type: :unknown_network_error,
              error: LogSanitizer.sanitize(error),
              context: sanitized_context
            )
        )

        {:error, :network_error}
    end
  end

  @doc """
  Handle packet processing errors with recovery strategies
  """
  def handle_packet_error(error, packet_data, context \\ %{}) do
    _sanitized_packet = LogSanitizer.sanitize_packet_data(packet_data)
    sanitized_context = LogSanitizer.sanitize_error_context(context)

    case error do
      %Jason.DecodeError{} ->
        Logger.warning("Invalid JSON in packet data",
          packet_error:
            LogSanitizer.log_data(
              error_type: :invalid_json,
              packet_sender: Map.get(packet_data, :sender, "unknown"),
              context: sanitized_context
            )
        )

        store_bad_packet(packet_data, "invalid_json")
        {:error, :invalid_json}

      %ArgumentError{message: message} ->
        if String.contains?(message, "invalid") do
          Logger.warning("Invalid packet format",
            packet_error:
              LogSanitizer.log_data(
                error_type: :invalid_format,
                packet_sender: Map.get(packet_data, :sender, "unknown"),
                message: message,
                context: sanitized_context
              )
          )

          store_bad_packet(packet_data, "invalid_format")
          {:error, :invalid_format}
        else
          Logger.error("Unexpected packet processing error",
            packet_error:
              LogSanitizer.log_data(
                error_type: :processing_error,
                packet_sender: Map.get(packet_data, :sender, "unknown"),
                error: LogSanitizer.sanitize(message),
                context: sanitized_context
              )
          )

          store_bad_packet(packet_data, "processing_error")
          {:error, :processing_failed}
        end

      error ->
        Logger.error("Unexpected packet processing error",
          packet_error:
            LogSanitizer.log_data(
              error_type: :processing_error,
              packet_sender: Map.get(packet_data, :sender, "unknown"),
              error: LogSanitizer.sanitize(error),
              context: sanitized_context
            )
        )

        store_bad_packet(packet_data, "processing_error")
        {:error, :processing_failed}
    end
  end

  @doc """
  Handle GenServer and process errors
  """
  def handle_process_error(error, process_name, context \\ %{}) do
    sanitized_context = LogSanitizer.sanitize_error_context(context)

    case error do
      :timeout ->
        Logger.warning("Process operation timeout",
          process_error:
            LogSanitizer.log_data(
              process: process_name,
              error_type: :timeout,
              context: sanitized_context
            )
        )

        {:error, :timeout}

      {:noproc, _} ->
        Logger.error("Process not found or died",
          process_error:
            LogSanitizer.log_data(
              process: process_name,
              error_type: :process_died,
              context: sanitized_context
            )
        )

        {:error, :process_unavailable}

      {:badrpc, reason} ->
        Logger.error("RPC call failed",
          process_error:
            LogSanitizer.log_data(
              process: process_name,
              error_type: :rpc_failed,
              reason: reason,
              context: sanitized_context
            )
        )

        {:error, :rpc_failed}

      error ->
        Logger.error("Unexpected process error",
          process_error:
            LogSanitizer.log_data(
              process: process_name,
              error_type: :unknown_process_error,
              error: LogSanitizer.sanitize(error),
              context: sanitized_context
            )
        )

        {:error, :process_error}
    end
  end

  @doc """
  Execute function with comprehensive error handling and recovery
  """
  def with_error_handling(fun, opts \\ []) when is_function(fun, 0) do
    context = Keyword.get(opts, :context, %{})
    max_retries = Keyword.get(opts, :max_retries, 0)
    retry_delay = Keyword.get(opts, :retry_delay, 1000)

    do_with_retries(fun, max_retries, retry_delay, context, 0)
  end

  defp do_with_retries(fun, max_retries, retry_delay, context, attempt) do
    {:ok, fun.()}
  rescue
    error ->
      if attempt < max_retries do
        Logger.warning("Retrying operation after error",
          retry_info:
            LogSanitizer.log_data(
              attempt: attempt + 1,
              max_retries: max_retries,
              delay_ms: retry_delay,
              error: LogSanitizer.sanitize(error)
            )
        )

        Process.sleep(retry_delay)
        do_with_retries(fun, max_retries, retry_delay, context, attempt + 1)
      else
        categorize_and_handle_error(error, context)
      end
  catch
    kind, error ->
      Logger.error("Unexpected error in operation",
        error_details:
          LogSanitizer.log_data(
            kind: kind,
            error: LogSanitizer.sanitize(error),
            context: LogSanitizer.sanitize_error_context(context)
          )
      )

      {:error, :unexpected_error}
  end

  defp categorize_and_handle_error(error, context) do
    cond do
      is_database_error?(error) ->
        handle_database_error(error, context)

      is_network_error?(error) ->
        handle_network_error(error, :unknown_service, context)

      true ->
        Logger.error("Uncategorized error",
          error_details:
            LogSanitizer.log_data(
              error: LogSanitizer.sanitize(error),
              context: LogSanitizer.sanitize_error_context(context)
            )
        )

        {:error, :unknown_error}
    end
  end

  defp is_database_error?(%CastError{}), do: true
  defp is_database_error?(%Ecto.ConstraintError{}), do: true
  defp is_database_error?(%Postgrex.Error{}), do: true
  defp is_database_error?(%DBConnection.ConnectionError{}), do: true
  defp is_database_error?(%Ecto.QueryError{}), do: true
  defp is_database_error?(_), do: false

  defp is_network_error?(%Req.TransportError{}), do: true
  defp is_network_error?(%{reason: :circuit_open}), do: true
  defp is_network_error?(%{reason: :timeout}), do: true
  defp is_network_error?(%RuntimeError{message: "circuit_open"}), do: true
  defp is_network_error?(%RuntimeError{message: "timeout"}), do: true
  defp is_network_error?(_), do: false

  defp store_bad_packet(packet_data, reason) do
    # This would store the bad packet for later analysis
    # For now, just log it since BadPacket module may not exist
    Logger.warning("Storing bad packet for analysis",
      bad_packet:
        LogSanitizer.log_data(
          raw_packet: inspect(packet_data),
          error_reason: reason,
          received_at: DateTime.utc_now()
        )
    )

    :ok
  end
end

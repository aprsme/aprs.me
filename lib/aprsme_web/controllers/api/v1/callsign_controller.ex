defmodule AprsmeWeb.Api.V1.CallsignController do
  @moduledoc """
  API v1 controller for callsign-related endpoints.
  """
  use AprsmeWeb, :controller

  alias Aprsme.CachedQueries
  alias Aprsme.ErrorHandler
  alias AprsmeWeb.Api.V1.CallsignJSON

  action_fallback AprsmeWeb.Api.V1.FallbackController

  @doc """
  Get the most recent packet for a given callsign.

  ## Parameters
    * `callsign` - The callsign to search for, with optional SSID (e.g., "N0CALL" or "N0CALL-9")

  ## Returns
    * 200 - Success with packet data
    * 404 - No packets found for the callsign
    * 400 - Invalid callsign format
  """
  def show(conn, %{"callsign" => callsign}) do
    with {:ok, normalized_callsign} <- validate_callsign(callsign),
         {:ok, packet} <- get_latest_packet(normalized_callsign) do
      conn
      |> put_status(:ok)
      |> put_view(json: CallsignJSON)
      |> render(:show, packet: packet)
    else
      {:error, :invalid_callsign} ->
        {:error, "Invalid callsign format"}

      {:error, :not_found} ->
        conn
        |> put_status(:ok)
        |> put_view(json: CallsignJSON)
        |> render(:not_found, callsign: callsign)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp validate_callsign(callsign) when is_binary(callsign) do
    # Remove any whitespace and convert to uppercase
    normalized = callsign |> String.trim() |> String.upcase()

    if validate_callsign_format(normalized) do
      {:ok, normalized}
    else
      {:error, :invalid_callsign}
    end
  end

  defp validate_callsign(nil), do: {:error, :invalid_callsign}
  defp validate_callsign(_), do: {:error, :invalid_callsign}

  defp validate_callsign_format(callsign) do
    # Basic callsign validation
    # Matches patterns like: N0CALL, N0CALL-9, W1ABC-15, etc.
    # This is a simplified regex - a full implementation might be more comprehensive
    callsign_regex = ~r/^[A-Z0-9]{1,3}[0-9][A-Z]{1,4}(-[0-9]{1,2})?$/

    String.match?(callsign, callsign_regex) and String.length(callsign) <= 12
  end

  defp get_latest_packet(callsign) do
    # Get the most recent packet for this callsign regardless of age or type
    # Use cached version for better performance with error handling
    fn ->
      case CachedQueries.get_latest_packet_for_callsign_cached(callsign) do
        nil -> {:error, :not_found}
        packet -> {:ok, packet}
      end
    end
    |> ErrorHandler.with_error_handling(
      context: %{callsign: callsign, operation: :get_latest_packet},
      max_retries: 1,
      retry_delay: 500
    )
    |> case do
      {:ok, result} -> result
      {:error, error} -> {:error, error}
    end
  end
end

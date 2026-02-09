defmodule Aprsme.LogSanitizer do
  @moduledoc """
  Utility module for sanitizing log output to prevent sensitive data leakage
  """

  @sensitive_fields [
    :password,
    :passcode,
    :auth_token,
    :api_key,
    :secret,
    :token,
    :authorization,
    :csrf_token,
    :session_token,
    :private_key,
    :database_url,
    :secret_key_base,
    :signing_salt
  ]

  @sensitive_patterns [
    ~r/password[=:]\s*\S+/i,
    ~r/passcode[=:]\s*\S+/i,
    ~r/token[=:]\s*\S+/i,
    ~r/key[=:]\s*\S+/i,
    ~r/secret[=:]\s*\S+/i,
    ~r/authorization[=:]\s*\S+/i
  ]

  @doc """
  Sanitize a map by removing or redacting sensitive fields
  """
  def sanitize_map(data) when is_map(data) do
    Enum.reduce(@sensitive_fields, data, fn field, acc ->
      cond do
        Map.has_key?(acc, field) -> Map.put(acc, field, "[REDACTED]")
        Map.has_key?(acc, to_string(field)) -> Map.put(acc, to_string(field), "[REDACTED]")
        true -> acc
      end
    end)
  end

  def sanitize_map(data), do: data

  # Helper to redact a matched sensitive pattern
  defp redact_match(match) do
    case String.split(match, ["=", ":"]) do
      [key, _value] -> "#{key}=[REDACTED]"
      _ -> "[REDACTED]"
    end
  end

  @doc """
  Sanitize a string by replacing sensitive patterns
  """
  def sanitize_string(data) when is_binary(data) do
    Enum.reduce(@sensitive_patterns, data, fn pattern, acc ->
      Regex.replace(pattern, acc, &redact_match/1)
    end)
  end

  def sanitize_string(data), do: data

  @doc """
  Sanitize any data structure recursively
  """
  def sanitize(data) when is_map(data) do
    data
    |> sanitize_map()
    |> Map.new(fn {k, v} -> {k, sanitize(v)} end)
  end

  def sanitize(data) when is_list(data) do
    Enum.map(data, &sanitize/1)
  end

  def sanitize(data) when is_binary(data) do
    sanitize_string(data)
  end

  def sanitize(data), do: data

  @doc """
  Sanitize packet data for logging
  """
  def sanitize_packet_data(packet_data) when is_map(packet_data) do
    # Remove potentially sensitive packet content but keep structure
    packet_data
    |> Map.drop([:raw_packet, :comment, :status, :data_extended])
    |> sanitize_map()
  end

  def sanitize_packet_data(data), do: data

  @doc """
  Sanitize connection information for logging
  """
  def sanitize_connection_info(conn_info) when is_map(conn_info) do
    conn_info
    |> Map.drop([:password, :passcode])
    |> Map.put(:password, "[REDACTED]")
    |> Map.put(:passcode, "[REDACTED]")
  end

  def sanitize_connection_info(data), do: data

  @doc """
  Sanitize error information for logging
  """
  def sanitize_error_context(context) when is_map(context) do
    context
    |> sanitize_map()
    |> Map.update(:stacktrace, nil, fn stacktrace ->
      if is_list(stacktrace) do
        # Limit stacktrace length
        Enum.take(stacktrace, 5)
      else
        stacktrace
      end
    end)
  end

  def sanitize_error_context(data), do: data

  @doc """
  Create a sanitized logging keyword list
  """
  def log_data(keyword_list) when is_list(keyword_list) do
    Enum.map(keyword_list, fn {key, value} ->
      {key, sanitize(value)}
    end)
  end

  def log_data(data), do: sanitize(data)
end

defmodule Aprsme.DeviceCache do
  @moduledoc """
  Caches device information and provides efficient lookup by identifier with wildcard matching.
  """

  use GenServer

  alias Aprsme.Devices
  alias Aprsme.Repo

  @cache_name :device_cache
  @refresh_interval to_timeout(day: 1)

  # Client API

  @doc """
  Starts the DeviceCache GenServer.
  """
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @doc """
  Looks up a device by identifier using wildcard matching.
  Returns the device struct if found, or nil.
  """
  def lookup_device(nil), do: nil

  def lookup_device(identifier) when is_binary(identifier) do
    case Cachex.get(@cache_name, :all_devices) do
      {:ok, nil} ->
        # Cache miss - load devices
        GenServer.call(__MODULE__, :refresh_cache)
        lookup_device(identifier)

      {:ok, devices} when is_list(devices) ->
        find_matching_device(devices, identifier)

      _ ->
        nil
    end
  end

  @doc """
  Refreshes the device cache from the database.
  """
  def refresh_cache do
    GenServer.call(__MODULE__, :refresh_cache)
  end

  # Server callbacks

  @impl true
  def init(_) do
    # Load devices on startup
    load_devices_into_cache()

    # Schedule periodic refresh
    Process.send_after(self(), :refresh_cache, @refresh_interval)

    {:ok, %{}}
  end

  @impl true
  def handle_call(:refresh_cache, _from, state) do
    result = load_devices_into_cache()
    {:reply, result, state}
  end

  @impl true
  def handle_info(:refresh_cache, state) do
    load_devices_into_cache()

    # Schedule next refresh
    Process.send_after(self(), :refresh_cache, @refresh_interval)

    {:noreply, state}
  end

  # Private functions

  defp load_devices_into_cache do
    devices = Repo.all(Devices)

    # Store all devices in cache
    case Cachex.put(@cache_name, :all_devices, devices) do
      {:ok, true} -> :ok
      error -> error
    end
  end

  defp find_matching_device(devices, identifier) do
    Enum.find(devices, fn device ->
      pattern = device.identifier

      cond do
        String.contains?(pattern, "?") ->
          try do
            regex = wildcard_pattern_to_regex(pattern)
            Regex.match?(regex, identifier)
          rescue
            _e in Regex.CompileError ->
              false
          end

        String.contains?(pattern, "*") ->
          # Compare literally if pattern contains * but not ?
          pattern == identifier

        true ->
          pattern == identifier
      end
    end)
  end

  # Converts a pattern with ? wildcards to a regex
  defp wildcard_pattern_to_regex(pattern) when is_binary(pattern) do
    # Replace ? with a placeholder, escape all regex metacharacters except the placeholder,
    # then replace placeholder with .
    pattern
    |> String.replace("?", "__WILDCARD__")
    # Escape all regex metacharacters
    |> String.replace(~r/([\\.\+\*\?\[\^\]\$\(\)\{\}=!<>\|:\-])/, "\\\\\\1")
    |> String.replace("__WILDCARD__", ".")
    |> then(fn s ->
      regex = "^" <> s <> "$"
      ~r/#{regex}/
    end)
  end
end

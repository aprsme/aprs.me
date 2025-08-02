defmodule Aprsme.RegexCache do
  @moduledoc """
  A simple in-memory cache for compiled regex patterns to avoid recompilation.
  Uses ETS for thread-safe access.
  """

  use GenServer

  require Logger

  @table_name :regex_cache
  @max_cache_size 1000

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  @impl true
  def init(_) do
    # Create ETS table for caching compiled regexes
    :ets.new(@table_name, [:set, :public, :named_table, read_concurrency: true])
    {:ok, %{}}
  end

  @doc """
  Get or compile a regex pattern. Returns {:ok, regex} or {:error, reason}.
  """
  def get_or_compile(pattern_string) do
    case :ets.lookup(@table_name, pattern_string) do
      [{^pattern_string, regex}] ->
        {:ok, regex}

      [] ->
        case Regex.compile(pattern_string) do
          {:ok, regex} ->
            # Check cache size and clear if needed
            if :ets.info(@table_name, :size) >= @max_cache_size do
              clear_oldest_entries()
            end

            :ets.insert(@table_name, {pattern_string, regex})
            {:ok, regex}

          error ->
            error
        end
    end
  end

  defp clear_oldest_entries do
    # Simple strategy: clear half the cache
    # In production, you might want LRU eviction
    entries = :ets.tab2list(@table_name)
    to_remove = Enum.take(entries, div(length(entries), 2))

    Enum.each(to_remove, fn {key, _} ->
      :ets.delete(@table_name, key)
    end)
  end
end

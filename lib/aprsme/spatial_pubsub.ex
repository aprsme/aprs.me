defmodule Aprsme.SpatialPubSub do
  @moduledoc """
  Spatial-aware PubSub system that broadcasts packets only to clients
  whose viewports contain the packet's location.
  """
  use GenServer

  alias Phoenix.PubSub

  require Logger

  # Grid size in degrees for spatial indexing
  @grid_size 1.0

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Register a client with their current viewport bounds.
  """
  def register_viewport(client_id, bounds) do
    GenServer.call(__MODULE__, {:register_viewport, client_id, bounds})
  end

  @doc """
  Update a client's viewport bounds.
  """
  def update_viewport(client_id, bounds) do
    GenServer.call(__MODULE__, {:update_viewport, client_id, bounds})
  end

  @doc """
  Unregister a client.
  """
  def unregister_client(client_id) do
    GenServer.cast(__MODULE__, {:unregister_client, client_id})
  end

  @doc """
  Broadcast a packet to all clients whose viewports contain the packet's location.
  """
  def broadcast_packet(packet) do
    GenServer.cast(__MODULE__, {:broadcast_packet, packet})
  end

  @doc """
  Get statistics about spatial filtering.
  """
  def get_stats do
    GenServer.call(__MODULE__, :get_stats)
  end

  @doc """
  Start telemetry reporting for LiveDashboard integration.
  """
  def start_telemetry_reporting do
    GenServer.cast(__MODULE__, :start_telemetry_reporting)
  end

  # Server callbacks

  @impl true
  def init(_opts) do
    # Subscribe to packet notifications
    PubSub.subscribe(Aprsme.PubSub, "postgres:aprsme_packets")

    # Start telemetry reporting
    Process.send_after(self(), :report_telemetry, 5_000)

    state = %{
      # client_id => %{bounds: bounds, topic: topic, pid: pid}
      clients: %{},
      # grid_key => MapSet of client_ids
      spatial_index: %{},
      # Statistics
      stats: %{
        total_broadcasts: 0,
        filtered_broadcasts: 0,
        total_packets: 0,
        clients_count: 0
      }
    }

    {:ok, state}
  end

  @impl true
  def handle_call({:register_viewport, client_id, bounds}, {pid, _}, state) do
    # Create a unique topic for this client
    topic = "spatial:#{client_id}"

    # Monitor the client process
    Process.monitor(pid)

    # Update client info
    client_info = %{
      bounds: normalize_bounds(bounds),
      topic: topic,
      pid: pid
    }

    # Update spatial index
    new_state =
      state
      |> put_in([:clients, client_id], client_info)
      |> update_spatial_index(client_id, client_info.bounds)
      |> update_in([:stats, :clients_count], &(&1 + 1))

    {:reply, {:ok, topic}, new_state}
  end

  @impl true
  def handle_call({:update_viewport, client_id, bounds}, _from, state) do
    case Map.get(state.clients, client_id) do
      nil ->
        {:reply, {:error, :not_registered}, state}

      client_info ->
        # Remove old spatial index entries
        old_bounds = client_info.bounds
        state = remove_from_spatial_index(state, client_id, old_bounds)

        # Update with new bounds
        normalized_bounds = normalize_bounds(bounds)
        updated_client = %{client_info | bounds: normalized_bounds}

        new_state =
          state
          |> put_in([:clients, client_id], updated_client)
          |> update_spatial_index(client_id, normalized_bounds)

        {:reply, :ok, new_state}
    end
  end

  @impl true
  def handle_call(:get_stats, _from, state) do
    stats =
      Map.merge(state.stats, %{
        grid_cells: map_size(state.spatial_index),
        avg_clients_per_cell: calculate_avg_clients_per_cell(state.spatial_index)
      })

    {:reply, stats, state}
  end

  @impl true
  def handle_cast({:unregister_client, client_id}, state) do
    new_state = remove_client(state, client_id)
    {:noreply, new_state}
  end

  @impl true
  def handle_cast({:broadcast_packet, packet}, state) do
    state = update_in(state, [:stats, :total_packets], &(&1 + 1))

    case extract_location(packet) do
      {lat, lon} when is_number(lat) and is_number(lon) ->
        # Find all clients whose viewports contain this location
        client_ids = find_clients_for_location(state, lat, lon)

        # Broadcast to each matching client's topic
        Enum.each(client_ids, fn client_id ->
          case Map.get(state.clients, client_id) do
            %{topic: topic} ->
              PubSub.broadcast(Aprsme.PubSub, topic, {:spatial_packet, packet})

            _ ->
              :ok
          end
        end)

        # Update statistics
        state =
          state
          |> update_in([:stats, :total_broadcasts], &(&1 + length(client_ids)))
          |> update_in([:stats, :filtered_broadcasts], &(&1 + max(0, map_size(state.clients) - length(client_ids))))

        {:noreply, state}

      _ ->
        # No valid location, skip broadcasting
        {:noreply, state}
    end
  end

  @impl true
  def handle_info({:postgres_packet, packet}, state) do
    # Handle packets from Postgres notifications
    handle_cast({:broadcast_packet, packet}, state)
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    # Find and remove the client associated with this pid
    client_id =
      Enum.find_value(state.clients, fn {id, %{pid: client_pid}} ->
        if client_pid == pid, do: id
      end)

    new_state =
      if client_id do
        remove_client(state, client_id)
      else
        state
      end

    {:noreply, new_state}
  end

  @impl true
  def handle_info(:report_telemetry, state) do
    # Emit telemetry metrics
    emit_telemetry_metrics(state)

    # Schedule next report
    Process.send_after(self(), :report_telemetry, 5_000)

    {:noreply, state}
  end

  @impl true
  def handle_info(_, state), do: {:noreply, state}

  # Private functions

  defp normalize_bounds(%{north: n, south: s, east: e, west: w}) do
    %{
      north: ensure_float(n),
      south: ensure_float(s),
      east: ensure_float(e),
      west: ensure_float(w)
    }
  end

  defp ensure_float(val) when is_binary(val), do: String.to_float(val)
  defp ensure_float(val) when is_integer(val), do: val * 1.0
  defp ensure_float(val) when is_float(val), do: val
  defp ensure_float(_), do: 0.0

  defp update_spatial_index(state, client_id, bounds) do
    # Get all grid cells that intersect with the bounds
    grid_cells = get_intersecting_grid_cells(bounds)

    # Add client to each grid cell
    Enum.reduce(grid_cells, state, fn grid_key, acc_state ->
      update_in(acc_state, [:spatial_index, grid_key], fn
        nil -> MapSet.new([client_id])
        set -> MapSet.put(set, client_id)
      end)
    end)
  end

  defp remove_from_spatial_index(state, client_id, bounds) do
    grid_cells = get_intersecting_grid_cells(bounds)

    Enum.reduce(grid_cells, state, fn grid_key, acc_state ->
      case Map.get(acc_state.spatial_index, grid_key) do
        nil ->
          acc_state

        set ->
          new_set = MapSet.delete(set, client_id)

          if MapSet.size(new_set) == 0 do
            # Remove the key entirely instead of storing nil
            update_in(acc_state, [:spatial_index], &Map.delete(&1, grid_key))
          else
            put_in(acc_state, [:spatial_index, grid_key], new_set)
          end
      end
    end)
  end

  defp get_intersecting_grid_cells(%{north: n, south: s, east: e, west: w}) do
    # Calculate which grid cells intersect with the bounds
    min_lat_cell = floor(s / @grid_size)
    max_lat_cell = floor(n / @grid_size)
    min_lon_cell = floor(w / @grid_size)
    max_lon_cell = floor(e / @grid_size)

    for lat_cell <- min_lat_cell..max_lat_cell,
        lon_cell <- min_lon_cell..max_lon_cell do
      {lat_cell, lon_cell}
    end
  end

  defp find_clients_for_location(state, lat, lon) do
    # Get the grid cell for this location
    grid_key = {floor(lat / @grid_size), floor(lon / @grid_size)}

    # Get all clients in this grid cell
    case Map.get(state.spatial_index, grid_key) do
      nil ->
        []

      client_set ->
        # Filter to only clients whose bounds actually contain the point
        Enum.filter(client_set, fn client_id ->
          case Map.get(state.clients, client_id) do
            %{bounds: bounds} -> point_in_bounds?(lat, lon, bounds)
            _ -> false
          end
        end)
    end
  end

  defp point_in_bounds?(lat, lon, %{north: n, south: s, east: e, west: w}) do
    lat >= s and lat <= n and lon >= w and lon <= e
  end

  defp extract_location(packet) do
    case packet do
      %{lat: lat, lon: lon} when not is_nil(lat) and not is_nil(lon) ->
        {ensure_float(lat), ensure_float(lon)}

      _ ->
        nil
    end
  end

  defp remove_client(state, client_id) do
    case Map.get(state.clients, client_id) do
      nil ->
        state

      %{bounds: bounds} ->
        state
        |> remove_from_spatial_index(client_id, bounds)
        |> update_in([:clients], &Map.delete(&1, client_id))
        |> update_in([:stats, :clients_count], &max(0, &1 - 1))
    end
  end

  defp calculate_avg_clients_per_cell(spatial_index) do
    non_nil_cells =
      spatial_index
      |> Map.values()
      |> Enum.reject(&is_nil/1)

    if Enum.empty?(non_nil_cells) do
      0.0
    else
      total_clients =
        non_nil_cells
        |> Enum.map(&MapSet.size/1)
        |> Enum.sum()

      total_clients / length(non_nil_cells)
    end
  end

  defp emit_telemetry_metrics(state) do
    # Client metrics
    :telemetry.execute(
      [:aprsme, :spatial_pubsub, :clients],
      %{
        count: state.stats.clients_count,
        grid_cells: map_size(state.spatial_index),
        avg_clients_per_cell: calculate_avg_clients_per_cell(state.spatial_index)
      },
      %{}
    )

    # Broadcast metrics
    :telemetry.execute(
      [:aprsme, :spatial_pubsub, :broadcasts],
      %{
        total: state.stats.total_broadcasts,
        filtered: state.stats.filtered_broadcasts,
        packets: state.stats.total_packets
      },
      %{}
    )

    # Calculate and emit efficiency metrics
    total_potential_broadcasts = state.stats.total_packets * state.stats.clients_count

    efficiency =
      if total_potential_broadcasts > 0 do
        1.0 - state.stats.total_broadcasts / total_potential_broadcasts
      else
        0.0
      end

    :telemetry.execute(
      [:aprsme, :spatial_pubsub, :efficiency],
      %{
        ratio: efficiency,
        saved_broadcasts: state.stats.filtered_broadcasts
      },
      %{}
    )
  end
end

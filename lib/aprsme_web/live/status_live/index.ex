defmodule AprsmeWeb.StatusLive.Index do
  @moduledoc """
  LiveView for displaying real-time APRS-IS connection status
  """
  use AprsmeWeb, :live_view

  # 5 seconds - reduced from 1 second to improve performance
  @refresh_interval 5_000

  @impl true
  def mount(_params, _session, socket) do
    # Load cached status on mount for fast initial load
    aprs_status = get_cached_aprs_status()

    socket =
      assign(socket,
        page_title: "System Status",
        aprs_status: aprs_status,
        current_time: DateTime.utc_now(),
        health_score: calculate_health_score(aprs_status),
        loading: false
      )

    if connected?(socket) do
      # Subscribe to status updates via PubSub
      Phoenix.PubSub.subscribe(Aprsme.PubSub, "aprs_status")

      # Schedule the first refresh with a slight delay
      Process.send_after(self(), :refresh_status, 500)
    end

    {:ok, socket}
  end

  @impl true
  def handle_info(:refresh_status, socket) do
    # Refresh status asynchronously
    self_pid = self()

    Task.start(fn ->
      status = get_aprs_status()
      send(self_pid, {:status_updated, status})
    end)

    # Schedule next refresh
    schedule_refresh()
    {:noreply, assign(socket, loading: true)}
  end

  @impl true
  def handle_info({:status_updated, status}, socket) do
    socket =
      assign(socket,
        aprs_status: status,
        current_time: DateTime.utc_now(),
        health_score: calculate_health_score(status),
        loading: false
      )

    {:noreply, socket}
  end

  @impl true
  def handle_info({:aprs_status_update, status}, socket) do
    # Handle real-time status updates via PubSub
    socket =
      assign(socket,
        aprs_status: status,
        current_time: DateTime.utc_now(),
        health_score: calculate_health_score(status)
      )

    {:noreply, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="container mx-auto max-w-4xl p-4">
      <div class="card bg-base-100 shadow-xl">
        <div class="card-body">
          <div class="flex justify-between items-center mb-6">
            <h1 class="card-title text-3xl">{gettext("APRS.me System Status")}</h1>
            <div class="text-sm opacity-70">
              {gettext("Last updated:")}
              <span class="font-mono">{Calendar.strftime(@current_time, "%H:%M:%S UTC")}</span>
            </div>
          </div>
          <!-- Overall Status Alert -->
          <%= if not @aprs_status.connected do %>
            <div class="alert alert-error mb-6">
              <svg
                xmlns="http://www.w3.org/2000/svg"
                class="stroke-current shrink-0 h-6 w-6"
                fill="none"
                viewBox="0 0 24 24"
              >
                <path
                  stroke-linecap="round"
                  stroke-linejoin="round"
                  stroke-width="2"
                  d="M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z"
                />
              </svg>
              <div>
                <h3 class="font-bold">{gettext("APRS-IS Connection Issue")}</h3>
                <div class="text-xs">
                  {gettext(
                    "The system is currently disconnected from the APRS-IS network. This may be due to network issues or server maintenance."
                  )}
                </div>
              </div>
            </div>
          <% end %>
          
    <!-- APRS-IS Connection Status -->
          <div class="mb-8">
            <h2 class="text-xl font-semibold mb-4">{gettext("APRS-IS Connection")}</h2>
            <div class="card bg-base-200">
              <div class="card-body">
                <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
                  <div class="flex items-center">
                    <span class="text-sm font-medium opacity-70 mr-2">{gettext("Status:")}</span>
                    <%= if @aprs_status.connected do %>
                      <div class="badge badge-success gap-1">
                        <div class="w-2 h-2 bg-current rounded-full"></div>
                        {gettext("Connected")}
                      </div>
                    <% else %>
                      <div class="badge badge-error gap-1">
                        <div class="w-2 h-2 bg-current rounded-full"></div>
                        {gettext("Disconnected")}
                      </div>
                    <% end %>
                  </div>

                  <div class="flex items-center">
                    <span class="text-sm font-medium opacity-70 mr-2">{gettext("Server:")}</span>
                    <span class="text-sm font-mono">
                      {@aprs_status.server}
                    </span>
                  </div>

                  <div class="flex items-center">
                    <span class="text-sm font-medium opacity-70 mr-2">{gettext("Login ID:")}</span>
                    <span class="text-sm font-mono">{@aprs_status.login_id}</span>
                  </div>

                  <div class="flex items-center">
                    <span class="text-sm font-medium opacity-70 mr-2">
                      {gettext("Connected Since:")}
                    </span>
                    <span class="text-sm">
                      <%= if @aprs_status.connected_at do %>
                        {Calendar.strftime(@aprs_status.connected_at, "%Y-%m-%d %H:%M:%S UTC")}
                      <% else %>
                        <span class="opacity-50">{gettext("Not connected")}</span>
                      <% end %>
                    </span>
                  </div>

                  <div class="flex items-center">
                    <span class="text-sm font-medium opacity-70 mr-2">{gettext("Uptime:")}</span>
                    <span class={[
                      "text-sm font-medium",
                      if(@aprs_status.connected, do: "text-success", else: "text-error")
                    ]}>
                      {format_uptime(@aprs_status.uptime_seconds)}
                    </span>
                  </div>

                  <div class="flex items-center col-span-2">
                    <span class="text-sm font-medium opacity-70 mr-2">{gettext("Filter:")}</span>
                    <div class="badge badge-outline font-mono">
                      {@aprs_status.filter}
                    </div>
                  </div>
                </div>
                
    <!-- Packet Statistics -->
                <div class="divider"></div>
                <div>
                  <h3 class="text-sm font-medium mb-3">{gettext("Packet Statistics")}</h3>
                  <div class="grid grid-cols-1 md:grid-cols-5 gap-4">
                    <div class="flex items-center">
                      <span class="text-sm font-medium opacity-70 mr-2">
                        {gettext("Total Packets:")}
                      </span>
                      <span class="text-sm font-mono">
                        {format_number(@aprs_status.packet_stats.total_packets)}
                      </span>
                    </div>

                    <div class="flex items-center">
                      <span class="text-sm font-medium opacity-70 mr-2">
                        {gettext("Packets/Sec:")}
                      </span>
                      <span class="text-sm font-mono">
                        {@aprs_status.packet_stats.packets_per_second}
                      </span>
                    </div>

                    <div class="flex items-center">
                      <span class="text-sm font-medium opacity-70 mr-2">
                        {gettext("Packets/Min:")}
                      </span>
                      <span class="text-sm font-mono">
                        {@aprs_status.packet_stats.packets_per_second * 60}
                      </span>
                    </div>

                    <div class="flex items-center">
                      <span class="text-sm font-medium opacity-70 mr-2">
                        {gettext("Last Packet:")}
                      </span>
                      <span class="text-sm">
                        <%= if @aprs_status.packet_stats.last_packet_at do %>
                          {format_time_ago(@aprs_status.packet_stats.last_packet_at)}
                        <% else %>
                          <span class="opacity-50">{gettext("None")}</span>
                        <% end %>
                      </span>
                    </div>

                    <div class="flex items-center">
                      <span class="text-sm font-medium opacity-70 mr-2">
                        {gettext("Stored Packets:")}
                      </span>
                      <span class="text-sm font-mono">
                        {format_number(@aprs_status.stored_packet_count)}
                      </span>
                    </div>
                  </div>

                  <div class="mt-2">
                    <div class="flex items-center">
                      <span class="text-sm font-medium opacity-70 mr-2">
                        {gettext("Oldest Packet:")}
                      </span>
                      <span class="text-sm">
                        <%= if @aprs_status[:oldest_packet_timestamp] do %>
                          <span class="font-mono">
                            {Calendar.strftime(
                              @aprs_status.oldest_packet_timestamp,
                              "%Y-%m-%d %H:%M:%S UTC"
                            )}
                          </span>
                          <span class="opacity-70">
                            ({format_time_ago(@aprs_status.oldest_packet_timestamp)})
                          </span>
                        <% else %>
                          <span class="opacity-50">{gettext("No packets stored")}</span>
                        <% end %>
                      </span>
                    </div>
                  </div>
                </div>
                
    <!-- Health Score -->
                <div class="divider"></div>
                <div class="flex items-center justify-between">
                  <span class="text-sm font-medium opacity-70">{gettext("Connection Health:")}</span>
                  <div class="flex items-center">
                    <div class="rating rating-sm mr-2">
                      <%= for i <- 1..5 do %>
                        <input
                          type="radio"
                          name="health-rating"
                          class="mask mask-star-2 bg-orange-400"
                          disabled
                          checked={i <= @health_score}
                        />
                      <% end %>
                    </div>
                    <span class="text-sm font-medium">
                      {@health_score}/5
                    </span>
                  </div>
                </div>
                <p class="text-xs opacity-70 mt-1">
                  {get_health_description(@health_score, @aprs_status.connected)}
                </p>
              </div>
            </div>
          </div>
          
    <!-- Cluster Information (if available) -->
          <%= if Map.has_key?(@aprs_status, :cluster_info) do %>
            <div class="mb-8">
              <h2 class="text-xl font-semibold mb-4">{gettext("Cluster Status")}</h2>
              <div class="card bg-base-200">
                <div class="card-body">
                  <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
                    <div class="flex items-center">
                      <span class="text-sm font-medium opacity-70 mr-2">{gettext("Total Nodes:")}</span>
                      <span class="text-sm font-mono">{@aprs_status.cluster_info.total_nodes}</span>
                    </div>

                    <div class="flex items-center">
                      <span class="text-sm font-medium opacity-70 mr-2">{gettext("Connected Nodes:")}</span>
                      <span class="text-sm font-mono">{@aprs_status.cluster_info.connected_nodes}</span>
                    </div>

                    <div class="flex items-center">
                      <span class="text-sm font-medium opacity-70 mr-2">{gettext("Leader Node:")}</span>
                      <span class="text-sm font-mono">{@aprs_status.cluster_info.leader_node}</span>
                    </div>
                  </div>

                  <div class="divider"></div>

                  <div class="flex items-center">
                    <span class="text-sm font-medium opacity-70 mr-2">{gettext("Cluster Mode:")}</span>
                    <div class="badge badge-success gap-1">
                      <div class="w-2 h-2 bg-current rounded-full"></div>
                      {gettext("Enabled")}
                    </div>
                  </div>

                  <%= if Map.has_key?(@aprs_status.cluster_info, :all_nodes) do %>
                    <div class="mt-4">
                      <span class="text-sm font-medium opacity-70">{gettext("Cluster Nodes:")}</span>
                      <div class="flex flex-wrap gap-2 mt-2">
                        <%= for node_name <- @aprs_status.cluster_info.all_nodes do %>
                          <div class={[
                            "badge gap-1",
                            if(node_name == @aprs_status.cluster_info.leader_node,
                              do: "badge-primary",
                              else: "badge-outline"
                            )
                          ]}>
                            <%= if node_name == @aprs_status.cluster_info.leader_node do %>
                              <div class="w-2 h-2 bg-current rounded-full"></div>
                            <% end %>
                            <span class="font-mono text-xs">{node_name}</span>
                          </div>
                        <% end %>
                      </div>
                      <p class="text-xs opacity-70 mt-1">
                        {gettext("Leader node")} <span class="badge badge-primary badge-xs">{gettext("highlighted")}</span>
                      </p>
                    </div>
                  <% end %>

                  <p class="text-xs opacity-70 mt-2">
                    {gettext(
                      "Status is aggregated from all nodes in the cluster. Only the leader node maintains the APRS-IS connection."
                    )}
                  </p>
                </div>
              </div>
            </div>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  # Private functions

  defp get_aprs_status do
    Aprsme.Cluster.LeaderElection.get_cluster_aprs_status()
  rescue
    error ->
      require Logger

      Logger.error("Error getting APRS status: #{inspect(error)}")
      # Return a default status when database is unavailable
      default_status()
  end

  defp get_cached_aprs_status do
    # Try to get cached status for instant load
    case Cachex.get(:query_cache, "aprs_status") do
      {:ok, status} when not is_nil(status) ->
        status

      _ ->
        # Fallback to direct query if cache miss
        status = get_aprs_status()
        Cachex.put(:query_cache, "aprs_status", status, ttl: to_timeout(second: 5))
        status
    end
  end

  defp default_status do
    %{
      connected: false,
      server: "unknown",
      port: 0,
      connected_at: nil,
      uptime_seconds: 0,
      login_id: "unknown",
      filter: "unknown",
      packet_stats: %{
        total_packets: 0,
        packets_per_second: 0,
        last_packet_at: nil
      },
      stored_packet_count: 0,
      oldest_packet_timestamp: nil
    }
  end

  defp schedule_refresh do
    Process.send_after(self(), :refresh_status, @refresh_interval)
  end

  def format_uptime(seconds) when seconds <= 0, do: gettext("Not connected")

  def format_uptime(seconds) do
    days = div(seconds, 86_400)
    hours = div(rem(seconds, 86_400), 3600)
    minutes = div(rem(seconds, 3600), 60)
    secs = rem(seconds, 60)

    cond do
      days > 0 -> "#{days}d #{hours}h #{minutes}m #{secs}s"
      hours > 0 -> "#{hours}h #{minutes}m #{secs}s"
      minutes > 0 -> "#{minutes}m #{secs}s"
      true -> "#{secs}s"
    end
  end

  defp calculate_health_score(aprs_status) do
    cond do
      not aprs_status.connected -> 1
      # Less than 5 minutes
      aprs_status.uptime_seconds < 300 -> 2
      # Less than 1 hour
      aprs_status.uptime_seconds < 3600 -> 3
      # Less than 1 day
      aprs_status.uptime_seconds < 86_400 -> 4
      # More than 1 day
      true -> 5
    end
  end

  def get_health_description(score, connected) do
    case {score, connected} do
      {1, false} -> gettext("Disconnected - Connection issues detected")
      {2, true} -> gettext("Recently connected - Monitoring stability")
      {3, true} -> gettext("Good - Connection stable for less than 1 hour")
      {4, true} -> gettext("Very good - Connection stable for less than 1 day")
      {5, true} -> gettext("Excellent - Long-term stable connection")
      _ -> gettext("Unknown status")
    end
  end

  def format_time_ago(nil), do: gettext("Never")

  def format_time_ago(datetime) do
    diff_seconds = DateTime.diff(DateTime.utc_now(), datetime)

    cond do
      diff_seconds < 60 -> gettext("%{count} seconds ago", count: diff_seconds)
      diff_seconds < 3600 -> gettext("%{count} minutes ago", count: div(diff_seconds, 60))
      diff_seconds < 86_400 -> gettext("%{count} hours ago", count: div(diff_seconds, 3600))
      true -> gettext("%{count} days ago", count: div(diff_seconds, 86_400))
    end
  end

  def format_number(number) when is_integer(number) do
    number
    |> Integer.to_string()
    |> String.reverse()
    |> String.replace(~r/(\d{3})(?=\d)/, "\\1,")
    |> String.reverse()
  end

  def format_number(number), do: to_string(number)
end

defmodule AprsmeWeb.StatusLive.Index do
  @moduledoc """
  LiveView for displaying real-time APRS-IS connection status
  """
  use AprsmeWeb, :live_view

  alias Aprsme.Cluster.LeaderElection

  require Logger

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
      try do
        status = get_aprs_status()
        send(self_pid, {:status_updated, status})
      rescue
        error ->
          Logger.error("Failed to refresh APRS status: #{inspect(error)}")
          # Send empty/default status on error
          send(
            self_pid,
            {:status_updated,
             %{
               connected: false,
               uptime_seconds: 0,
               data_rate: 0,
               packet_stats: %{
                 packets_per_second: 0,
                 last_packet_at: nil
               },
               stored_packet_count: 0,
               error: "Failed to fetch status"
             }}
          )
      end
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
    <div class="mx-auto max-w-4xl p-4 sm:px-6 lg:px-8">
      <div class="bg-white shadow-sm sm:rounded-lg dark:bg-gray-800/50 dark:shadow-none dark:outline dark:-outline-offset-1 dark:outline-white/10">
        <div class="px-6 py-8">
          <div class="flex justify-between items-center mb-6">
            <h1 class="text-3xl font-bold text-gray-900 dark:text-white">{gettext("APRS.me System Status")}</h1>
            <div class="text-sm text-gray-500 dark:text-gray-400">
              {gettext("Last updated:")}
              <span class="font-mono">{Calendar.strftime(@current_time, "%H:%M:%S UTC")}</span>
            </div>
          </div>
          <!-- Overall Status Alert -->
          <%= if not @aprs_status.connected do %>
            <div class="rounded-md bg-red-50 p-4 mb-6 dark:bg-red-500/10">
              <div class="flex">
                <svg
                  xmlns="http://www.w3.org/2000/svg"
                  class="h-5 w-5 text-red-400 shrink-0"
                  fill="none"
                  viewBox="0 0 24 24"
                  stroke="currentColor"
                >
                  <path
                    stroke-linecap="round"
                    stroke-linejoin="round"
                    stroke-width="2"
                    d="M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z"
                  />
                </svg>
                <div class="ml-3">
                  <h3 class="text-sm font-medium text-red-800 dark:text-red-300">{gettext("APRS-IS Connection Issue")}</h3>
                  <div class="mt-1 text-sm text-red-700 dark:text-red-400">
                    {gettext(
                      "The system is currently disconnected from the APRS-IS network. This may be due to network issues or server maintenance."
                    )}
                  </div>
                </div>
              </div>
            </div>
          <% end %>
          
    <!-- APRS-IS Connection Status -->
          <div class="mb-8">
            <h2 class="text-xl font-semibold mb-4 text-gray-900 dark:text-white">{gettext("APRS-IS Connection")}</h2>
            <div class="rounded-lg bg-gray-50 p-6 dark:bg-gray-700/50">
              <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div class="flex items-center">
                  <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">{gettext("Status:")}</span>
                  <%= if @aprs_status.connected do %>
                    <span class="inline-flex items-center gap-1 rounded-md bg-green-100 px-2 py-1 text-xs font-medium text-green-700 dark:bg-green-400/10 dark:text-green-400">
                      <span class="h-2 w-2 rounded-full bg-current"></span>
                      {gettext("Connected")}
                    </span>
                  <% else %>
                    <span class="inline-flex items-center gap-1 rounded-md bg-red-100 px-2 py-1 text-xs font-medium text-red-700 dark:bg-red-400/10 dark:text-red-400">
                      <span class="h-2 w-2 rounded-full bg-current"></span>
                      {gettext("Disconnected")}
                    </span>
                  <% end %>
                </div>

                <div class="flex items-center">
                  <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">{gettext("Server:")}</span>
                  <span class="text-sm font-mono text-gray-900 dark:text-white">
                    {@aprs_status.server}
                  </span>
                </div>

                <div class="flex items-center">
                  <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">{gettext("Login ID:")}</span>
                  <span class="text-sm font-mono text-gray-900 dark:text-white">{@aprs_status.login_id}</span>
                </div>

                <div class="flex items-center">
                  <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">
                    {gettext("Connected Since:")}
                  </span>
                  <span class="text-sm text-gray-900 dark:text-white">
                    <%= if @aprs_status.connected_at do %>
                      {Calendar.strftime(@aprs_status.connected_at, "%Y-%m-%d %H:%M:%S UTC")}
                    <% else %>
                      <span class="text-gray-400 dark:text-gray-500">{gettext("Not connected")}</span>
                    <% end %>
                  </span>
                </div>

                <div class="flex items-center">
                  <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">{gettext("Uptime:")}</span>
                  <span class={[
                    "text-sm font-medium",
                    if(@aprs_status.connected,
                      do: "text-green-600 dark:text-green-400",
                      else: "text-red-600 dark:text-red-400"
                    )
                  ]}>
                    {format_uptime(@aprs_status.uptime_seconds)}
                  </span>
                </div>

                <div class="flex items-center col-span-2">
                  <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">{gettext("Filter:")}</span>
                  <span class="inline-flex items-center rounded-md bg-gray-100 px-2 py-1 text-xs font-medium font-mono text-gray-600 dark:bg-gray-400/10 dark:text-gray-400">
                    {@aprs_status.filter}
                  </span>
                </div>
              </div>
              
    <!-- Packet Statistics -->
              <div class="border-t border-gray-200 dark:border-white/10 my-4"></div>
              <div>
                <h3 class="text-sm font-medium mb-3 text-gray-900 dark:text-white">{gettext("Packet Statistics")}</h3>
                <div class="grid grid-cols-1 md:grid-cols-5 gap-4">
                  <div class="flex items-center">
                    <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">
                      {gettext("Total Packets:")}
                    </span>
                    <span class="text-sm font-mono text-gray-900 dark:text-white">
                      {format_number(@aprs_status.packet_stats.total_packets)}
                    </span>
                  </div>

                  <div class="flex items-center">
                    <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">
                      {gettext("Packets/Sec:")}
                    </span>
                    <span class="text-sm font-mono text-gray-900 dark:text-white">
                      {@aprs_status.packet_stats.packets_per_second}
                    </span>
                  </div>

                  <div class="flex items-center">
                    <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">
                      {gettext("Packets/Min:")}
                    </span>
                    <span class="text-sm font-mono text-gray-900 dark:text-white">
                      {@aprs_status.packet_stats.packets_per_second * 60}
                    </span>
                  </div>

                  <div class="flex items-center">
                    <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">
                      {gettext("Last Packet:")}
                    </span>
                    <span class="text-sm text-gray-900 dark:text-white">
                      <%= if @aprs_status.packet_stats.last_packet_at do %>
                        {format_time_ago(@aprs_status.packet_stats.last_packet_at)}
                      <% else %>
                        <span class="text-gray-400 dark:text-gray-500">{gettext("None")}</span>
                      <% end %>
                    </span>
                  </div>

                  <div class="flex items-center">
                    <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">
                      {gettext("Stored Packets:")}
                    </span>
                    <span class="text-sm font-mono text-gray-900 dark:text-white">
                      {format_number(@aprs_status.stored_packet_count)}
                    </span>
                  </div>
                </div>

                <div class="mt-2">
                  <div class="flex items-center">
                    <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">
                      {gettext("Oldest Packet:")}
                    </span>
                    <span class="text-sm text-gray-900 dark:text-white">
                      <%= if @aprs_status[:oldest_packet_timestamp] do %>
                        <span class="font-mono">
                          {Calendar.strftime(
                            @aprs_status.oldest_packet_timestamp,
                            "%Y-%m-%d %H:%M:%S UTC"
                          )}
                        </span>
                        <span class="text-gray-500 dark:text-gray-400">
                          ({format_time_ago(@aprs_status.oldest_packet_timestamp)})
                        </span>
                      <% else %>
                        <span class="text-gray-400 dark:text-gray-500">{gettext("No packets stored")}</span>
                      <% end %>
                    </span>
                  </div>
                </div>
              </div>
              
    <!-- Health Score -->
              <div class="border-t border-gray-200 dark:border-white/10 my-4"></div>
              <div class="flex items-center justify-between">
                <span class="text-sm font-medium text-gray-500 dark:text-gray-400">{gettext("Connection Health:")}</span>
                <div class="flex items-center gap-1">
                  <%= for i <- 1..5 do %>
                    <svg
                      class={[
                        "h-5 w-5",
                        if(i <= @health_score, do: "text-amber-400", else: "text-gray-300 dark:text-gray-600")
                      ]}
                      viewBox="0 0 20 20"
                      fill="currentColor"
                    >
                      <path
                        fill-rule="evenodd"
                        d="M10.868 2.884c-.321-.772-1.415-.772-1.736 0l-1.83 4.401-4.753.381c-.833.067-1.171 1.107-.536 1.651l3.62 3.102-1.106 4.637c-.194.813.691 1.456 1.405 1.02L10 15.591l4.069 2.485c.713.436 1.598-.207 1.404-1.02l-1.106-4.637 3.62-3.102c.635-.544.297-1.584-.536-1.65l-4.752-.382-1.831-4.401z"
                        clip-rule="evenodd"
                      />
                    </svg>
                  <% end %>
                  <span class="ml-1 text-sm font-medium text-gray-900 dark:text-white">
                    {@health_score}/5
                  </span>
                </div>
              </div>
              <p class="text-xs text-gray-500 dark:text-gray-400 mt-1">
                {get_health_description(@health_score, @aprs_status.connected)}
              </p>
            </div>
          </div>
          
    <!-- Cluster Information (if available) -->
          <%= if Map.has_key?(@aprs_status, :cluster_info) do %>
            <div class="mb-8">
              <h2 class="text-xl font-semibold mb-4 text-gray-900 dark:text-white">{gettext("Cluster Status")}</h2>
              <div class="rounded-lg bg-gray-50 p-6 dark:bg-gray-700/50">
                <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
                  <div class="flex items-center">
                    <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">{gettext("Total Nodes:")}</span>
                    <span class="text-sm font-mono text-gray-900 dark:text-white">
                      {@aprs_status.cluster_info.total_nodes}
                    </span>
                  </div>

                  <div class="flex items-center">
                    <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">
                      {gettext("Connected Nodes:")}
                    </span>
                    <span class="text-sm font-mono text-gray-900 dark:text-white">
                      {@aprs_status.cluster_info.connected_nodes}
                    </span>
                  </div>

                  <div class="flex items-center">
                    <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">{gettext("Leader Node:")}</span>
                    <span class="text-sm font-mono text-gray-900 dark:text-white">
                      {@aprs_status.cluster_info.leader_node}
                    </span>
                  </div>
                </div>

                <div class="border-t border-gray-200 dark:border-white/10 my-4"></div>

                <div class="flex items-center">
                  <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">{gettext("Cluster Mode:")}</span>
                  <span class="inline-flex items-center gap-1 rounded-md bg-green-100 px-2 py-1 text-xs font-medium text-green-700 dark:bg-green-400/10 dark:text-green-400">
                    <span class="h-2 w-2 rounded-full bg-current"></span>
                    {gettext("Enabled")}
                  </span>
                </div>

                <%= if Map.has_key?(@aprs_status.cluster_info, :all_nodes) do %>
                  <div class="mt-4">
                    <span class="text-sm font-medium text-gray-500 dark:text-gray-400">{gettext("Cluster Nodes:")}</span>
                    <div class="flex flex-wrap gap-2 mt-2">
                      <%= for node_name <- @aprs_status.cluster_info.all_nodes do %>
                        <span class={[
                          "inline-flex items-center gap-1 rounded-md px-2 py-1 text-xs font-medium font-mono",
                          if(node_name == @aprs_status.cluster_info.leader_node,
                            do: "bg-indigo-100 text-indigo-700 dark:bg-indigo-400/10 dark:text-indigo-400",
                            else: "bg-gray-100 text-gray-600 dark:bg-gray-400/10 dark:text-gray-400"
                          )
                        ]}>
                          <%= if node_name == @aprs_status.cluster_info.leader_node do %>
                            <span class="h-2 w-2 rounded-full bg-current"></span>
                          <% end %>
                          {node_name}
                        </span>
                      <% end %>
                    </div>
                    <p class="text-xs text-gray-500 dark:text-gray-400 mt-1">
                      {gettext("Leader node")}
                      <span class="inline-flex items-center rounded-md bg-indigo-100 px-1.5 py-0.5 text-xs font-medium text-indigo-700 dark:bg-indigo-400/10 dark:text-indigo-400">
                        {gettext("highlighted")}
                      </span>
                    </p>
                  </div>
                <% end %>

                <p class="text-xs text-gray-500 dark:text-gray-400 mt-2">
                  {gettext(
                    "Status is aggregated from all nodes in the cluster. Only the leader node maintains the APRS-IS connection."
                  )}
                </p>
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
    LeaderElection.get_cluster_aprs_status()
  rescue
    error ->
      require Logger

      Logger.error("Error getting APRS status: #{inspect(error)}")
      # Return a default status when database is unavailable
      default_status()
  end

  defp get_cached_aprs_status do
    # Try to get cached status for instant load
    case Aprsme.Cache.get(:query_cache, "aprs_status") do
      {:ok, status} when not is_nil(status) ->
        status

      _ ->
        # Fallback to direct query if cache miss
        status = get_aprs_status()
        Aprsme.Cache.put(:query_cache, "aprs_status", status)
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

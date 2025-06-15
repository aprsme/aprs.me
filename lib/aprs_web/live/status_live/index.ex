defmodule AprsWeb.StatusLive.Index do
  @moduledoc """
  LiveView for displaying real-time APRS-IS connection status
  """
  use AprsWeb, :live_view

  # 30 seconds
  @refresh_interval 30_000

  @impl true
  def mount(_params, _session, socket) do
    socket =
      assign(socket,
        page_title: "System Status",
        aprs_status: get_aprs_status(),
        version: get_app_version(),
        current_time: DateTime.utc_now(),
        auto_refresh: true,
        health_score: calculate_health_score(get_aprs_status())
      )

    if connected?(socket) do
      # Schedule the first refresh
      schedule_refresh()
    end

    {:ok, socket}
  end

  @impl true
  def handle_event("toggle_auto_refresh", _params, socket) do
    new_auto_refresh = !socket.assigns.auto_refresh

    socket = assign(socket, auto_refresh: new_auto_refresh)

    if new_auto_refresh do
      schedule_refresh()
    end

    {:noreply, socket}
  end

  @impl true
  def handle_event("refresh_now", _params, socket) do
    socket = refresh_status(socket)
    {:noreply, socket}
  end

  @impl true
  def handle_info(:refresh_status, socket) do
    socket = refresh_status(socket)

    # Schedule next refresh if auto-refresh is enabled
    if socket.assigns.auto_refresh do
      schedule_refresh()
    end

    {:noreply, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="mx-auto max-w-4xl">
      <div class="bg-white shadow rounded-lg">
        <div class="px-4 py-5 sm:p-6">
          <div class="flex justify-between items-center mb-6">
            <h1 class="text-3xl font-bold text-gray-900">APRS.me System Status</h1>
            <div class="text-sm text-gray-500">
              Last updated:
              <span class="font-mono">{Calendar.strftime(@current_time, "%H:%M:%S UTC")}</span>
            </div>
          </div>
          
    <!-- Overall Status Alert -->
          <%= if not @aprs_status.connected do %>
            <div class="mb-6 rounded-md bg-red-50 p-4 border border-red-200">
              <div class="flex">
                <div class="flex-shrink-0">
                  <svg
                    class="h-5 w-5 text-red-400"
                    xmlns="http://www.w3.org/2000/svg"
                    viewBox="0 0 20 20"
                    fill="currentColor"
                  >
                    <path
                      fill-rule="evenodd"
                      d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.28 7.22a.75.75 0 00-1.06 1.06L8.94 10l-1.72 1.72a.75.75 0 101.06 1.06L10 11.06l1.72 1.72a.75.75 0 101.06-1.06L11.06 10l1.72-1.72a.75.75 0 00-1.06-1.06L10 8.94 8.28 7.22z"
                      clip-rule="evenodd"
                    />
                  </svg>
                </div>
                <div class="ml-3">
                  <h3 class="text-sm font-medium text-red-800">
                    APRS-IS Connection Issue
                  </h3>
                  <div class="mt-2 text-sm text-red-700">
                    <p>
                      The system is currently disconnected from the APRS-IS network. This may be due to network issues or server maintenance.
                    </p>
                  </div>
                </div>
              </div>
            </div>
          <% end %>
          
    <!-- APRS-IS Connection Status -->
          <div class="mb-8">
            <h2 class="text-xl font-semibold text-gray-900 mb-4">APRS-IS Connection</h2>
            <div class="bg-gray-50 p-4 rounded-lg">
              <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div class="flex items-center">
                  <span class="text-sm font-medium text-gray-500 mr-2">Status:</span>
                  <%= if @aprs_status.connected do %>
                    <span class="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
                      <svg class="w-2 h-2 mr-1" fill="currentColor" viewBox="0 0 8 8">
                        <circle cx="4" cy="4" r="3" />
                      </svg>
                      Connected
                    </span>
                  <% else %>
                    <span class="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
                      <svg class="w-2 h-2 mr-1" fill="currentColor" viewBox="0 0 8 8">
                        <circle cx="4" cy="4" r="3" />
                      </svg>
                      Disconnected
                    </span>
                  <% end %>
                </div>

                <div class="flex items-center">
                  <span class="text-sm font-medium text-gray-500 mr-2">Server:</span>
                  <span class="text-sm text-gray-900 font-mono">
                    {@aprs_status.server}:{@aprs_status.port}
                  </span>
                </div>

                <div class="flex items-center">
                  <span class="text-sm font-medium text-gray-500 mr-2">Login ID:</span>
                  <span class="text-sm text-gray-900 font-mono">{@aprs_status.login_id}</span>
                </div>

                <div class="flex items-center">
                  <span class="text-sm font-medium text-gray-500 mr-2">Connected Since:</span>
                  <span class="text-sm text-gray-900">
                    <%= if @aprs_status.connected_at do %>
                      {Calendar.strftime(@aprs_status.connected_at, "%Y-%m-%d %H:%M:%S UTC")}
                    <% else %>
                      <span class="text-gray-400">Not connected</span>
                    <% end %>
                  </span>
                </div>

                <div class="flex items-center">
                  <span class="text-sm font-medium text-gray-500 mr-2">Uptime:</span>
                  <span class={[
                    "text-sm font-medium",
                    if(@aprs_status.connected, do: "text-green-700", else: "text-red-600")
                  ]}>
                    {format_uptime(@aprs_status.uptime_seconds)}
                  </span>
                </div>

                <div class="flex items-center col-span-2">
                  <span class="text-sm font-medium text-gray-500 mr-2">Filter:</span>
                  <span class="text-sm text-gray-900 font-mono bg-gray-100 px-2 py-1 rounded border">
                    {@aprs_status.filter}
                  </span>
                </div>
              </div>
              
    <!-- Packet Statistics -->
              <div class="mt-4 pt-4 border-t border-gray-200">
                <h3 class="text-sm font-medium text-gray-700 mb-3">Packet Statistics</h3>
                <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
                  <div class="flex items-center">
                    <span class="text-sm font-medium text-gray-500 mr-2">Total Packets:</span>
                    <span class="text-sm text-gray-900 font-mono">
                      {format_number(@aprs_status.packet_stats.total_packets)}
                    </span>
                  </div>

                  <div class="flex items-center">
                    <span class="text-sm font-medium text-gray-500 mr-2">Packets/Min:</span>
                    <span class="text-sm text-gray-900 font-mono">
                      {@aprs_status.packet_stats.packets_per_minute}
                    </span>
                  </div>

                  <div class="flex items-center">
                    <span class="text-sm font-medium text-gray-500 mr-2">Last Packet:</span>
                    <span class="text-sm text-gray-900">
                      <%= if @aprs_status.packet_stats.last_packet_at do %>
                        {format_time_ago(@aprs_status.packet_stats.last_packet_at)}
                      <% else %>
                        <span class="text-gray-400">None</span>
                      <% end %>
                    </span>
                  </div>
                </div>
              </div>
              
    <!-- Health Score -->
              <div class="mt-4 pt-4 border-t border-gray-200">
                <div class="flex items-center justify-between">
                  <span class="text-sm font-medium text-gray-500">Connection Health:</span>
                  <div class="flex items-center">
                    <div class="flex items-center mr-2">
                      <%= for i <- 1..5 do %>
                        <svg
                          class={[
                            "w-4 h-4",
                            if(i <= @health_score, do: "text-green-400", else: "text-gray-300")
                          ]}
                          fill="currentColor"
                          viewBox="0 0 20 20"
                        >
                          <path d="M9.049 2.927c.3-.921 1.603-.921 1.902 0l1.07 3.292a1 1 0 00.95.69h3.462c.969 0 1.371 1.24.588 1.81l-2.8 2.034a1 1 0 00-.364 1.118l1.07 3.292c.3.921-.755 1.688-1.54 1.118l-2.8-2.034a1 1 0 00-1.175 0l-2.8 2.034c-.784.57-1.838-.197-1.539-1.118l1.07-3.292a1 1 0 00-.364-1.118L2.98 8.72c-.783-.57-.38-1.81.588-1.81h3.461a1 1 0 00.951-.69l1.07-3.292z" />
                        </svg>
                      <% end %>
                    </div>
                    <span class="text-sm font-medium text-gray-700">
                      {@health_score}/5
                    </span>
                  </div>
                </div>
                <p class="mt-1 text-xs text-gray-500">
                  {get_health_description(@health_score, @aprs_status.connected)}
                </p>
              </div>
            </div>
          </div>
          
    <!-- Application Information -->
          <div class="mb-8">
            <h2 class="text-xl font-semibold text-gray-900 mb-4">Application Information</h2>
            <div class="bg-gray-50 p-4 rounded-lg">
              <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div class="flex items-center">
                  <span class="text-sm font-medium text-gray-500 mr-2">Version:</span>
                  <span class="text-sm text-gray-900 font-mono">{@version}</span>
                </div>

                <div class="flex items-center">
                  <span class="text-sm font-medium text-gray-500 mr-2">Current Time:</span>
                  <span class="text-sm text-gray-900 font-mono">
                    {Calendar.strftime(@current_time, "%Y-%m-%d %H:%M:%S UTC")}
                  </span>
                </div>
              </div>
            </div>
          </div>
          
    <!-- Auto-refresh controls -->
          <div class="bg-blue-50 p-4 rounded-lg">
            <div class="flex items-center justify-between">
              <div class="flex items-center">
                <div class="flex-shrink-0">
                  <svg
                    class="h-5 w-5 text-blue-400"
                    xmlns="http://www.w3.org/2000/svg"
                    viewBox="0 0 20 20"
                    fill="currentColor"
                  >
                    <path
                      fill-rule="evenodd"
                      d="M4 2a1 1 0 011 1v2.101a7.002 7.002 0 0111.601 2.566 1 1 0 11-1.885.666A5.002 5.002 0 005.999 7H9a1 1 0 010 2H4a1 1 0 01-1-1V3a1 1 0 011-1zm.008 9.057a1 1 0 011.276.61A5.002 5.002 0 0014.001 13H11a1 1 0 110-2h5a1 1 0 011 1v5a1 1 0 11-2 0v-2.101a7.002 7.002 0 01-11.601-2.566 1 1 0 01.61-1.276z"
                      clip-rule="evenodd"
                    />
                  </svg>
                </div>
                <div class="ml-3">
                  <p class="text-sm text-blue-700">
                    Auto-refresh:
                    <span class="font-semibold">
                      {if @auto_refresh, do: "ON", else: "OFF"}
                    </span>
                    <%= if @auto_refresh do %>
                      (every 30 seconds)
                    <% end %>
                  </p>
                </div>
              </div>
              <div class="flex space-x-2">
                <button
                  phx-click="toggle_auto_refresh"
                  class={[
                    "inline-flex items-center px-3 py-1 border text-xs font-medium rounded focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500",
                    if(@auto_refresh,
                      do: "border-blue-200 text-blue-700 bg-blue-100 hover:bg-blue-200",
                      else: "border-gray-300 text-gray-700 bg-white hover:bg-gray-50"
                    )
                  ]}
                >
                  {if @auto_refresh, do: "Disable", else: "Enable"} Auto-refresh
                </button>
                <button
                  phx-click="refresh_now"
                  class="inline-flex items-center px-3 py-1 border border-transparent text-xs font-medium rounded text-white bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500"
                >
                  <svg class="w-3 h-3 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path
                      stroke-linecap="round"
                      stroke-linejoin="round"
                      stroke-width="2"
                      d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"
                    />
                  </svg>
                  Refresh Now
                </button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  # Private functions

  defp get_aprs_status do
    Aprs.Is.get_status()
  end

  defp get_app_version do
    :aprs |> Application.spec(:vsn) |> List.to_string()
  end

  defp refresh_status(socket) do
    aprs_status = get_aprs_status()

    assign(socket,
      aprs_status: aprs_status,
      current_time: DateTime.utc_now(),
      health_score: calculate_health_score(aprs_status)
    )
  end

  defp schedule_refresh do
    Process.send_after(self(), :refresh_status, @refresh_interval)
  end

  defp format_uptime(seconds) when seconds <= 0, do: "Not connected"

  defp format_uptime(seconds) do
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

  defp get_health_description(score, connected) do
    case {score, connected} do
      {1, false} -> "Disconnected - Connection issues detected"
      {2, true} -> "Recently connected - Monitoring stability"
      {3, true} -> "Good - Connection stable for less than 1 hour"
      {4, true} -> "Very good - Connection stable for less than 1 day"
      {5, true} -> "Excellent - Long-term stable connection"
      _ -> "Unknown status"
    end
  end

  defp format_time_ago(datetime) do
    diff_seconds = DateTime.diff(DateTime.utc_now(), datetime)

    cond do
      diff_seconds < 60 -> "#{diff_seconds} seconds ago"
      diff_seconds < 3600 -> "#{div(diff_seconds, 60)} minutes ago"
      diff_seconds < 86_400 -> "#{div(diff_seconds, 3600)} hours ago"
      true -> "#{div(diff_seconds, 86_400)} days ago"
    end
  end

  defp format_number(number) when is_integer(number) do
    number
    |> Integer.to_string()
    |> String.reverse()
    |> String.replace(~r/(\d{3})(?=\d)/, "\\1,")
    |> String.reverse()
  end

  defp format_number(number), do: to_string(number)
end

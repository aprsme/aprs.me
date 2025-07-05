defmodule AprsmeWeb.StatusLive.ConnectionCardComponent do
  @moduledoc false
  use Phoenix.LiveComponent

  import Phoenix.Component

  def render(assigns) do
    ~H"""
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
              {AprsmeWeb.StatusLive.Index.format_uptime(@aprs_status.uptime_seconds)}
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
          <div class="grid grid-cols-1 md:grid-cols-4 gap-4">
            <div class="flex items-center">
              <span class="text-sm font-medium text-gray-500 mr-2">Total Packets:</span>
              <span class="text-sm text-gray-900 font-mono">
                {AprsmeWeb.StatusLive.Index.format_number(@aprs_status.packet_stats.total_packets)}
              </span>
            </div>

            <div class="flex items-center">
              <span class="text-sm font-medium text-gray-500 mr-2">Packets/Sec:</span>
              <span class="text-sm text-gray-900 font-mono">
                {@aprs_status.packet_stats.packets_per_second}
              </span>
            </div>

            <div class="flex items-center">
              <span class="text-sm font-medium text-gray-500 mr-2">Last Packet:</span>
              <span class="text-sm text-gray-900">
                <%= if @aprs_status.packet_stats.last_packet_at do %>
                  {AprsmeWeb.StatusLive.Index.format_time_ago(
                    @aprs_status.packet_stats.last_packet_at
                  )}
                <% else %>
                  <span class="text-gray-400">None</span>
                <% end %>
              </span>
            </div>

            <div class="flex items-center">
              <span class="text-sm font-medium text-gray-500 mr-2">Stored Packets:</span>
              <span class="text-sm text-gray-900 font-mono">
                {AprsmeWeb.StatusLive.Index.format_number(@aprs_status.stored_packet_count)}
              </span>
            </div>
          </div>
        </div>

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
            {AprsmeWeb.StatusLive.Index.get_health_description(@health_score, @aprs_status.connected)}
          </p>
        </div>
      </div>
    </div>
    """
  end
end

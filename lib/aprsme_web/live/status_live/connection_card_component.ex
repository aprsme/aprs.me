defmodule AprsmeWeb.StatusLive.ConnectionCardComponent do
  @moduledoc false
  use Phoenix.LiveComponent

  import Phoenix.Component

  def render(assigns) do
    ~H"""
    <div class="mb-8">
      <h2 class="text-xl font-semibold mb-4">APRS-IS Connection</h2>
      <div class="rounded-lg bg-gray-50 p-6 dark:bg-gray-700/50">
        <div>
          <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div class="flex items-center">
              <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">Status:</span>
              <%= if @aprs_status.connected do %>
                <span class="inline-flex items-center gap-x-1.5 rounded-md bg-green-100 px-2 py-1 text-xs font-medium text-green-700 dark:bg-green-400/10 dark:text-green-400">
                  <svg class="h-1.5 w-1.5 fill-green-500" viewBox="0 0 6 6" aria-hidden="true">
                    <circle cx="3" cy="3" r="3" />
                  </svg>
                  Connected
                </span>
              <% else %>
                <span class="inline-flex items-center gap-x-1.5 rounded-md bg-red-100 px-2 py-1 text-xs font-medium text-red-700 dark:bg-red-400/10 dark:text-red-400">
                  <svg class="h-1.5 w-1.5 fill-red-500" viewBox="0 0 6 6" aria-hidden="true">
                    <circle cx="3" cy="3" r="3" />
                  </svg>
                  Disconnected
                </span>
              <% end %>
            </div>

            <div class="flex items-center">
              <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">Server:</span>
              <span class="text-sm font-mono">
                {@aprs_status.server}:{@aprs_status.port}
              </span>
            </div>

            <div class="flex items-center">
              <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">Login ID:</span>
              <span class="text-sm font-mono">{@aprs_status.login_id}</span>
            </div>

            <div class="flex items-center">
              <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">Connected Since:</span>
              <span class="text-sm">
                <%= if @aprs_status.connected_at do %>
                  {Calendar.strftime(@aprs_status.connected_at, "%Y-%m-%d %H:%M:%S UTC")}
                <% else %>
                  <span class="text-gray-400 dark:text-gray-500">Not connected</span>
                <% end %>
              </span>
            </div>

            <div class="flex items-center">
              <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">Uptime:</span>
              <span class={[
                "text-sm font-medium",
                if(@aprs_status.connected, do: "text-green-600 dark:text-green-400", else: "text-red-600 dark:text-red-400")
              ]}>
                {AprsmeWeb.StatusLive.Index.format_uptime(@aprs_status.uptime_seconds)}
              </span>
            </div>

            <div class="flex items-center col-span-2">
              <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">Filter:</span>
              <span class="inline-flex items-center rounded-md bg-gray-100 px-2 py-1 text-xs font-medium text-gray-600 font-mono dark:bg-gray-400/10 dark:text-gray-400">
                {@aprs_status.filter}
              </span>
            </div>
          </div>
          
    <!-- Packet Statistics -->
          <div class="border-t border-gray-200 dark:border-white/10 my-4"></div>
          <div>
            <h3 class="text-sm font-medium mb-3">Packet Statistics</h3>
            <div class="grid grid-cols-1 md:grid-cols-5 gap-4">
              <div class="flex items-center">
                <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">Total Packets:</span>
                <span class="text-sm font-mono">
                  {AprsmeWeb.StatusLive.Index.format_number(@aprs_status.packet_stats.total_packets)}
                </span>
              </div>

              <div class="flex items-center">
                <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">Packets/Sec:</span>
                <span class="text-sm font-mono">
                  {@aprs_status.packet_stats.packets_per_second}
                </span>
              </div>

              <div class="flex items-center">
                <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">Packets/Min:</span>
                <span class="text-sm font-mono">
                  {@aprs_status.packet_stats.packets_per_second * 60}
                </span>
              </div>

              <div class="flex items-center">
                <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">Last Packet:</span>
                <span class="text-sm">
                  <%= if @aprs_status.packet_stats.last_packet_at do %>
                    {AprsmeWeb.StatusLive.Index.format_time_ago(@aprs_status.packet_stats.last_packet_at)}
                  <% else %>
                    <span class="text-gray-400 dark:text-gray-500">None</span>
                  <% end %>
                </span>
              </div>

              <div class="flex items-center">
                <span class="text-sm font-medium text-gray-500 dark:text-gray-400 mr-2">Stored Packets:</span>
                <span class="text-sm font-mono">
                  {AprsmeWeb.StatusLive.Index.format_number(@aprs_status.stored_packet_count)}
                </span>
              </div>
            </div>
          </div>

          <div class="border-t border-gray-200 dark:border-white/10 my-4"></div>
          <div class="flex items-center justify-between">
            <span class="text-sm font-medium text-gray-500 dark:text-gray-400">Connection Health:</span>
            <div class="flex items-center">
              <div class="flex items-center mr-2">
                <%= for i <- 1..5 do %>
                  <svg
                    class={"h-4 w-4 #{if i <= @health_score, do: "text-amber-400", else: "text-gray-300 dark:text-gray-600"}"}
                    viewBox="0 0 20 20"
                    fill="currentColor"
                    aria-hidden="true"
                  >
                    <path
                      fill-rule="evenodd"
                      d="M10.868 2.884c-.321-.772-1.415-.772-1.736 0l-1.83 4.401-4.753.381c-.833.067-1.171 1.107-.536 1.651l3.62 3.102-1.106 4.637c-.194.813.691 1.456 1.405 1.02L10 15.591l4.069 2.485c.713.436 1.598-.207 1.404-1.02l-1.106-4.637 3.62-3.102c.635-.544.297-1.584-.536-1.65l-4.752-.382-1.831-4.401z"
                      clip-rule="evenodd"
                    />
                  </svg>
                <% end %>
              </div>
              <span class="text-sm font-medium">
                {@health_score}/5
              </span>
            </div>
          </div>
          <p class="text-xs text-gray-500 dark:text-gray-400 mt-1">
            {AprsmeWeb.StatusLive.Index.get_health_description(@health_score, @aprs_status.connected)}
          </p>
        </div>
      </div>
    </div>
    """
  end
end

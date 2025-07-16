defmodule AprsmeWeb.StatusLive.ConnectionCardComponent do
  @moduledoc false
  use Phoenix.LiveComponent

  import Phoenix.Component

  def render(assigns) do
    ~H"""
    <div class="mb-8">
      <h2 class="text-xl font-semibold mb-4">APRS-IS Connection</h2>
      <div class="card bg-base-200">
        <div class="card-body">
          <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div class="flex items-center">
              <span class="text-sm font-medium opacity-70 mr-2">Status:</span>
              <%= if @aprs_status.connected do %>
                <div class="badge badge-success gap-1">
                  <div class="w-2 h-2 bg-current rounded-full"></div>
                  Connected
                </div>
              <% else %>
                <div class="badge badge-error gap-1">
                  <div class="w-2 h-2 bg-current rounded-full"></div>
                  Disconnected
                </div>
              <% end %>
            </div>

            <div class="flex items-center">
              <span class="text-sm font-medium opacity-70 mr-2">Server:</span>
              <span class="text-sm font-mono">
                {@aprs_status.server}:{@aprs_status.port}
              </span>
            </div>

            <div class="flex items-center">
              <span class="text-sm font-medium opacity-70 mr-2">Login ID:</span>
              <span class="text-sm font-mono">{@aprs_status.login_id}</span>
            </div>

            <div class="flex items-center">
              <span class="text-sm font-medium opacity-70 mr-2">Connected Since:</span>
              <span class="text-sm">
                <%= if @aprs_status.connected_at do %>
                  {Calendar.strftime(@aprs_status.connected_at, "%Y-%m-%d %H:%M:%S UTC")}
                <% else %>
                  <span class="opacity-50">Not connected</span>
                <% end %>
              </span>
            </div>

            <div class="flex items-center">
              <span class="text-sm font-medium opacity-70 mr-2">Uptime:</span>
              <span class={[
                "text-sm font-medium",
                if(@aprs_status.connected, do: "text-success", else: "text-error")
              ]}>
                {AprsmeWeb.StatusLive.Index.format_uptime(@aprs_status.uptime_seconds)}
              </span>
            </div>

            <div class="flex items-center col-span-2">
              <span class="text-sm font-medium opacity-70 mr-2">Filter:</span>
              <div class="badge badge-outline font-mono">
                {@aprs_status.filter}
              </div>
            </div>
          </div>
          
    <!-- Packet Statistics -->
          <div class="divider"></div>
          <div>
            <h3 class="text-sm font-medium mb-3">Packet Statistics</h3>
            <div class="grid grid-cols-1 md:grid-cols-5 gap-4">
              <div class="flex items-center">
                <span class="text-sm font-medium opacity-70 mr-2">Total Packets:</span>
                <span class="text-sm font-mono">
                  {AprsmeWeb.StatusLive.Index.format_number(@aprs_status.packet_stats.total_packets)}
                </span>
              </div>

              <div class="flex items-center">
                <span class="text-sm font-medium opacity-70 mr-2">Packets/Sec:</span>
                <span class="text-sm font-mono">
                  {@aprs_status.packet_stats.packets_per_second}
                </span>
              </div>

              <div class="flex items-center">
                <span class="text-sm font-medium opacity-70 mr-2">Packets/Min:</span>
                <span class="text-sm font-mono">
                  {@aprs_status.packet_stats.packets_per_second * 60}
                </span>
              </div>

              <div class="flex items-center">
                <span class="text-sm font-medium opacity-70 mr-2">Last Packet:</span>
                <span class="text-sm">
                  <%= if @aprs_status.packet_stats.last_packet_at do %>
                    {AprsmeWeb.StatusLive.Index.format_time_ago(@aprs_status.packet_stats.last_packet_at)}
                  <% else %>
                    <span class="opacity-50">None</span>
                  <% end %>
                </span>
              </div>

              <div class="flex items-center">
                <span class="text-sm font-medium opacity-70 mr-2">Stored Packets:</span>
                <span class="text-sm font-mono">
                  {AprsmeWeb.StatusLive.Index.format_number(@aprs_status.stored_packet_count)}
                </span>
              </div>
            </div>
          </div>

          <div class="divider"></div>
          <div class="flex items-center justify-between">
            <span class="text-sm font-medium opacity-70">Connection Health:</span>
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
            {AprsmeWeb.StatusLive.Index.get_health_description(@health_score, @aprs_status.connected)}
          </p>
        </div>
      </div>
    </div>
    """
  end
end

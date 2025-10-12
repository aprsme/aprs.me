defmodule AprsmeWeb.MapLive.Components do
  @moduledoc """
  Reusable function components for the Map LiveView.
  Extracted to improve maintainability and reduce the main LiveView module size.
  """
  use AprsmeWeb, :html

  attr :flash, :map, default: %{}
  attr :slideover_open, :boolean, default: false
  attr :map_center, :map, required: true
  attr :map_zoom, :integer, required: true
  attr :rest, :global

  def map_container(assigns) do
    ~H"""
    <div
      id="aprs-map"
      phx-update="ignore"
      phx-hook="APRSMap"
      data-center={Jason.encode!(@map_center)}
      data-zoom={@map_zoom}
      class={[
        @slideover_open && "slideover-open",
        !@slideover_open && "slideover-closed"
      ]}
    >
    </div>
    """
  end

  attr :slideover_open, :boolean, required: true
  attr :loading, :boolean, default: false
  attr :connection_status, :string, default: "pending"
  attr :packets, :list, default: []
  attr :show_all_packets, :boolean, default: true
  attr :tracked_callsign, :string, default: ""
  attr :tracked_callsign_latest_packet, :map, default: nil
  attr :rest, :global

  def slideover_panel(assigns) do
    ~H"""
    <div class={[
      "slideover-panel",
      @slideover_open && "slideover-open",
      !@slideover_open && "slideover-closed"
    ]}>
      <.slideover_header {assigns} />
      <.slideover_content {assigns} />
    </div>
    """
  end

  defp slideover_header(assigns) do
    ~H"""
    <div class="flex items-center justify-between p-4 border-b bg-gray-50">
      <h2 class="text-lg font-semibold text-gray-900">APRS Packets</h2>
      <button
        phx-click="toggle_slideover"
        class="p-2 text-gray-400 hover:text-gray-600 transition-colors"
      >
        <.icon name="hero-x-mark" class="w-5 h-5" />
      </button>
    </div>
    """
  end

  defp slideover_content(assigns) do
    ~H"""
    <div class="flex-1 overflow-hidden flex flex-col">
      <%= if @loading do %>
        <.loading_indicator />
      <% else %>
        <.packet_controls {assigns} />
        <.packet_list {assigns} />
      <% end %>
    </div>
    """
  end

  defp loading_indicator(assigns) do
    ~H"""
    <div class="flex items-center justify-center h-full">
      <div class="text-center">
        <div class="inline-flex items-center">
          <svg class="animate-spin h-8 w-8 text-blue-600" xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24">
            <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
            <path
              class="opacity-75"
              fill="currentColor"
              d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
            >
            </path>
          </svg>
          <span class="ml-3 text-lg text-gray-700">Loading packets...</span>
        </div>
      </div>
    </div>
    """
  end

  defp packet_controls(assigns) do
    ~H"""
    <div class="p-4 border-b space-y-3">
      <.connection_indicator connection_status={@connection_status} />
      <.view_toggle show_all_packets={@show_all_packets} />
      <.callsign_filter
        tracked_callsign={@tracked_callsign}
        tracked_callsign_latest_packet={@tracked_callsign_latest_packet}
      />
    </div>
    """
  end

  defp connection_indicator(assigns) do
    ~H"""
    <div class="flex items-center space-x-2">
      <div class={[
        "h-3 w-3 rounded-full",
        @connection_status == "connected" && "bg-green-500",
        @connection_status == "connecting" && "bg-yellow-500",
        @connection_status == "disconnected" && "bg-red-500",
        @connection_status == "pending" && "bg-gray-400"
      ]}>
      </div>
      <span class="text-sm text-gray-700">
        <%= case @connection_status do %>
          <% "connected" -> %>
            Connected to APRS-IS
          <% "connecting" -> %>
            Connecting to APRS-IS...
          <% "disconnected" -> %>
            Disconnected from APRS-IS
          <% _ -> %>
            Initializing...
        <% end %>
      </span>
    </div>
    """
  end

  defp view_toggle(assigns) do
    ~H"""
    <div class="flex items-center space-x-4">
      <label class="inline-flex items-center">
        <input
          type="radio"
          name="view_mode"
          value="all"
          checked={@show_all_packets}
          phx-click="toggle_view_mode"
          phx-value-mode="all"
          class="text-blue-600"
        />
        <span class="ml-2 text-sm">All Packets</span>
      </label>
      <label class="inline-flex items-center">
        <input
          type="radio"
          name="view_mode"
          value="with_position"
          checked={!@show_all_packets}
          phx-click="toggle_view_mode"
          phx-value-mode="with_position"
          class="text-blue-600"
        />
        <span class="ml-2 text-sm">With Position</span>
      </label>
    </div>
    """
  end

  defp callsign_filter(assigns) do
    ~H"""
    <div>
      <form phx-submit="filter_callsign" class="relative">
        <input
          type="text"
          name="callsign"
          value={@tracked_callsign}
          placeholder="Filter by callsign..."
          class="w-full px-3 py-2 border border-gray-300 rounded-md pr-8 text-sm focus:ring-blue-500 focus:border-blue-500"
          phx-debounce="300"
          phx-change="filter_callsign"
        />
        <%= if @tracked_callsign != "" do %>
          <button
            type="button"
            phx-click="clear_callsign_filter"
            class="absolute right-2 top-1/2 -translate-y-1/2 text-gray-400 hover:text-gray-600"
          >
            <.icon name="hero-x-mark" class="w-4 h-4" />
          </button>
        <% end %>
      </form>
      <%= if @tracked_callsign_latest_packet do %>
        <div class="mt-2 text-xs text-gray-600">
          Last seen: {format_time_ago(@tracked_callsign_latest_packet.received_at)}
        </div>
      <% end %>
    </div>
    """
  end

  defp packet_list(assigns) do
    ~H"""
    <div class="flex-1 overflow-y-auto p-4">
      <div id="packets" class="space-y-3" phx-update="stream">
        <div :for={{id, packet} <- @packets} id={id}>
          <.packet_card packet={packet} />
        </div>
      </div>
    </div>
    """
  end

  defp packet_card(assigns) do
    ~H"""
    <div class="bg-white border border-gray-200 rounded-lg p-4 hover:border-blue-300 transition-colors">
      <div class="flex items-start justify-between">
        <div class="flex-1">
          <div class="font-semibold text-gray-900">
            {@packet.sender}
            <%= if @packet.ssid && @packet.ssid != "0" do %>
              <span class="text-gray-500">-{@packet.ssid}</span>
            <% end %>
          </div>
          <%= if @packet.has_position do %>
            <div class="text-sm text-gray-600 mt-1">
              📍 {format_coordinates(@packet.lat, @packet.lon)}
            </div>
          <% end %>
          <%= if @packet.comment do %>
            <div class="text-sm text-gray-700 mt-2">
              {@packet.comment}
            </div>
          <% end %>
          <div class="text-xs text-gray-500 mt-2">
            via {@packet.path || "direct"} • {format_time_ago(@packet.received_at)}
          </div>
        </div>
        <%= if @packet.has_position do %>
          <button
            phx-click="center_on_packet"
            phx-value-lat={@packet.lat}
            phx-value-lon={@packet.lon}
            phx-value-sender={@packet.sender}
            class="ml-2 p-2 text-blue-600 hover:bg-blue-50 rounded transition-colors"
            title="Center map on this location"
          >
            <.icon name="hero-map-pin" class="w-5 h-5" />
          </button>
        <% end %>
      </div>
    </div>
    """
  end

  # Helper functions
  defp format_time_ago(datetime) do
    # Implement time ago formatting
    case DateTime.diff(DateTime.utc_now(), datetime, :second) do
      seconds when seconds < 60 -> "#{seconds}s ago"
      seconds when seconds < 3600 -> "#{div(seconds, 60)}m ago"
      seconds when seconds < 86_400 -> "#{div(seconds, 3600)}h ago"
      seconds -> "#{div(seconds, 86_400)}d ago"
    end
  end

  defp format_coordinates(lat, lon) when is_number(lat) and is_number(lon) do
    "#{Float.round(lat * 1.0, 4)}, #{Float.round(lon * 1.0, 4)}"
  end

  defp format_coordinates(_, _), do: "Unknown location"

  # Style component for better organization
  def map_styles(assigns) do
    ~H"""
    <style>
      #aprs-map {
        position: absolute;
        top: 0;
        left: 0;
        bottom: 0;
        height: 100vh;
        z-index: 1;
        transition: right 0.3s ease-in-out;
      }

      /* Desktop styles */
      @media (min-width: 1024px) {
        #aprs-map.slideover-open {
          right: 352px;
        }

        #aprs-map.slideover-closed {
          right: 0;
        }
      }

      /* Slideover panel base styles */
      .slideover-panel {
        position: fixed;
        top: 0;
        right: 0;
        bottom: 0;
        width: 352px;
        background: white;
        box-shadow: -2px 0 8px rgba(0, 0, 0, 0.1);
        z-index: 50;
        display: flex;
        flex-direction: column;
        transition: transform 0.3s ease-in-out;
        overflow: hidden;
      }

      .slideover-panel.slideover-open {
        transform: translateX(0);
      }

      .slideover-panel.slideover-closed {
        transform: translateX(100%);
      }

      /* Mobile responsiveness */
      @media (max-width: 1023px) {
        #aprs-map {
          right: 0 !important;
        }

        .slideover-panel {
          width: 100%;
          max-width: 400px;
        }
      }

      /* Loading spinner animation */
      @keyframes spin {
        to { transform: rotate(360deg); }
      }

      .animate-spin {
        animation: spin 1s linear infinite;
      }

      /* Custom scrollbar for packet list */
      #packets::-webkit-scrollbar {
        width: 6px;
      }

      #packets::-webkit-scrollbar-track {
        background: #f1f1f1;
      }

      #packets::-webkit-scrollbar-thumb {
        background: #888;
        border-radius: 3px;
      }

      #packets::-webkit-scrollbar-thumb:hover {
        background: #555;
      }
    </style>
    """
  end
end

defmodule AprsmeWeb.MapLive.PopupComponent do
  @moduledoc false
  use Phoenix.Component

  import AprsmeWeb.TimeHelpers

  def popup(assigns) do
    ~H"""
    <div class="aprs-popup" data-timestamp={@cache_buster}>
      <div class="aprs-callsign">
        <strong><.link navigate={"/#{@callsign}"}>{@callsign}</.link></strong>
        <.link navigate={"/info/#{@callsign}"} class="aprs-info-link">info</.link>
        <%= if @weather_link do %>
          <.link navigate={"/weather/#{@callsign}"} class="aprs-info-link">weather charts</.link>
        <% end %>
      </div>
      <%= if @weather do %>
        <div class="aprs-comment">Weather Report</div>
      <% else %>
        <%= if @comment != "" do %>
          <div class="aprs-comment">{@comment}</div>
        <% end %>
      <% end %>
      <%= if @timestamp_dt do %>
        <div class="aprs-timestamp">
          <div>{time_ago_in_words(@timestamp_dt)}</div>
          <div class="text-slate-400">
            {Calendar.strftime(@timestamp_dt, "%Y-%m-%d %H:%M:%S UTC")}
          </div>
        </div>
      <% end %>
      <%= if @weather do %>
        <hr style="margin-top: 4px; margin-bottom: 4px;" /> Temperature: {@temperature}°F<br />
        Humidity: {@humidity}%<br />
        Wind: {@wind_direction}° at {@wind_speed} mph, gusts to {@wind_gust} mph<br />
        Pressure: {@pressure} hPa<br /> Rain (1h): {@rain_1h} in.<br />
        Rain (24h): {@rain_24h} in.<br /> Rain (since midnight): {@rain_since_midnight} in.<br />
      <% end %>
    </div>
    """
  end
end

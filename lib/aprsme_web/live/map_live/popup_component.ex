defmodule AprsmeWeb.MapLive.PopupComponent do
  @moduledoc false
  use Phoenix.Component

  import AprsmeWeb.TimeHelpers
  import Gettext

  def popup(assigns) do
    ~H"""
    <div class="aprs-popup" data-timestamp={@cache_buster}>
      <div class="aprs-callsign">
        <strong><.link navigate={"/?call=#{@callsign}"}>{@callsign}</.link></strong>
        <.link navigate={"/info/#{@callsign}"} class="aprs-info-link">
          {gettext(AprsmeWeb.Gettext, "info")}
        </.link>
        <%= if @weather_link do %>
          <.link navigate={"/weather/#{@callsign}"} class="aprs-info-link">
            {gettext(AprsmeWeb.Gettext, "weather charts")}
          </.link>
        <% end %>
      </div>
      <%= if @weather do %>
        <div class="aprs-comment">{gettext(AprsmeWeb.Gettext, "Weather Report")}</div>
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
        <hr style="margin-top: 4px; margin-bottom: 4px;" />
        {gettext(AprsmeWeb.Gettext, "Temperature:")} {@temperature}{@temp_unit}<br />
        {gettext(AprsmeWeb.Gettext, "Humidity:")} {@humidity}%<br />
        {gettext(AprsmeWeb.Gettext, "Wind:")} {@wind_direction}° {gettext(AprsmeWeb.Gettext, "at")} {@wind_speed} {@wind_unit}, {gettext(
          AprsmeWeb.Gettext,
          "gusts to"
        )} {@wind_gust} {@gust_unit}<br />
        {gettext(AprsmeWeb.Gettext, "Pressure:")} {@pressure} hPa<br />
        {gettext(AprsmeWeb.Gettext, "Rain (1h):")} {@rain_1h} {@rain_1h_unit}<br />
        {gettext(AprsmeWeb.Gettext, "Rain (24h):")} {@rain_24h} {@rain_24h_unit}<br />
        {gettext(AprsmeWeb.Gettext, "Rain (since midnight):")} {@rain_since_midnight} {@rain_since_midnight_unit}<br />
      <% end %>
    </div>
    """
  end
end

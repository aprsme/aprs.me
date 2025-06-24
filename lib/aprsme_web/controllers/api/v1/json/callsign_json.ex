defmodule AprsmeWeb.Api.V1.CallsignJSON do
  @moduledoc """
  Renders callsign and packet data for API v1.
  """

  alias Aprsme.Packet

  def render("show.json", %{packet: packet}) do
    %{data: packet_json(packet)}
  end

  def render("index.json", %{packets: packets}) do
    %{data: Enum.map(packets, &packet_json/1)}
  end

  def render("not_found.json", %{callsign: callsign}) do
    %{
      data: nil,
      message: "No packets found for callsign #{callsign}"
    }
  end

  defp packet_json(%Packet{} = packet) do
    %{
      id: packet.id,
      callsign: format_callsign(packet.base_callsign, packet.ssid),
      base_callsign: packet.base_callsign,
      ssid: packet.ssid,
      sender: packet.sender,
      destination: packet.destination,
      path: packet.path,
      data_type: packet.data_type,
      information_field: packet.information_field,
      raw_packet: packet.raw_packet,
      received_at: packet.received_at,
      region: packet.region,
      position: position_json(packet),
      symbol: symbol_json(packet),
      comment: packet.comment,
      timestamp: packet.timestamp,
      aprs_messaging: packet.aprsme_messaging,
      weather: weather_json(packet),
      equipment: equipment_json(packet),
      message: message_json(packet),
      has_position: packet.has_position,
      inserted_at: packet.inserted_at,
      updated_at: packet.updated_at
    }
  end

  defp format_callsign(base_callsign, nil), do: base_callsign
  defp format_callsign(base_callsign, "0"), do: base_callsign
  defp format_callsign(base_callsign, ssid), do: "#{base_callsign}-#{ssid}"

  defp position_json(%Packet{has_position: false}), do: nil
  defp position_json(%Packet{lat: nil, lon: nil}), do: nil

  defp position_json(%Packet{} = packet) do
    base_position = %{
      latitude: to_float(packet.lat),
      longitude: to_float(packet.lon)
    }

    # Add course, speed, and altitude if available
    base_position
    |> maybe_add(:course, packet.course)
    |> maybe_add(:speed, packet.speed)
    |> maybe_add(:altitude, packet.altitude)
  end

  defp symbol_json(%Packet{symbol_code: nil, symbol_table_id: nil}), do: nil

  defp symbol_json(%Packet{} = packet) do
    %{
      code: packet.symbol_code,
      table_id: packet.symbol_table_id
    }
  end

  defp weather_json(%Packet{} = packet) do
    weather_data =
      %{}
      |> maybe_add(:temperature, packet.temperature)
      |> maybe_add(:humidity, packet.humidity)
      |> maybe_add(:wind_speed, packet.wind_speed)
      |> maybe_add(:wind_direction, packet.wind_direction)
      |> maybe_add(:wind_gust, packet.wind_gust)
      |> maybe_add(:pressure, packet.pressure)
      |> maybe_add(:rain_1h, packet.rain_1h)
      |> maybe_add(:rain_24h, packet.rain_24h)
      |> maybe_add(:rain_since_midnight, packet.rain_since_midnight)

    case weather_data do
      empty when empty == %{} -> nil
      data -> data
    end
  end

  defp equipment_json(%Packet{} = packet) do
    equipment_data =
      %{}
      |> maybe_add(:manufacturer, packet.manufacturer)
      |> maybe_add(:equipment_type, packet.equipment_type)

    case equipment_data do
      empty when empty == %{} -> nil
      data -> data
    end
  end

  defp message_json(%Packet{addressee: nil, message_text: nil, message_number: nil}), do: nil

  defp message_json(%Packet{} = packet) do
    %{}
    |> maybe_add(:addressee, packet.addressee)
    |> maybe_add(:text, packet.message_text)
    |> maybe_add(:number, packet.message_number)
  end

  defp maybe_add(map, _key, nil), do: map
  defp maybe_add(map, _key, ""), do: map
  defp maybe_add(map, key, value), do: Map.put(map, key, value)

  defp to_float(%Decimal{} = decimal), do: Decimal.to_float(decimal)
  defp to_float(value) when is_number(value), do: value
  defp to_float(_), do: nil
end

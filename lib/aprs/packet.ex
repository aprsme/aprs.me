defmodule Aprs.Packet do
  @moduledoc false
  use Aprs.Schema

  import Ecto.Changeset

  alias Aprs.DataExtended
  alias Parser.Types.MicE

  schema "packets" do
    field(:base_callsign, :string)
    field(:data_type, :string)
    field(:destination, :string)
    field(:information_field, :string)
    field(:path, :string)
    field(:sender, :string)
    field(:ssid, :string)
    field(:received_at, :utc_datetime_usec)
    field(:region, :string)
    field(:lat, :float)
    field(:lon, :float)
    field(:has_position, :boolean, default: false)

    # Original raw packet and symbol information
    field(:raw_packet, :string)
    field(:symbol_code, :string)
    field(:symbol_table_id, :string)

    # Additional packet data
    field(:comment, :string)
    field(:timestamp, :string)
    field(:aprs_messaging, :boolean, default: false)

    # Weather data
    field(:temperature, :float)
    field(:humidity, :float)
    field(:wind_speed, :float)
    field(:wind_direction, :integer)
    field(:wind_gust, :float)
    field(:pressure, :float)
    field(:rain_1h, :float)
    field(:rain_24h, :float)
    field(:rain_since_midnight, :float)

    # Equipment/status information
    field(:manufacturer, :string)
    field(:equipment_type, :string)
    field(:course, :integer)
    field(:speed, :float)
    field(:altitude, :float)

    # Message-specific fields
    field(:addressee, :string)
    field(:message_text, :string)
    field(:message_number, :string)

    embeds_one(:data_extended, DataExtended)

    timestamps()
  end

  @doc false
  def changeset(packet, attrs) do
    # Convert atom data_type to string
    attrs = normalize_data_type(attrs)

    packet
    |> cast(attrs, [
      :base_callsign,
      :data_type,
      :destination,
      :information_field,
      :path,
      :sender,
      :ssid,
      :received_at,
      :region,
      :lat,
      :lon,
      :has_position,
      :raw_packet,
      :symbol_code,
      :symbol_table_id,
      :comment,
      :timestamp,
      :aprs_messaging,
      :temperature,
      :humidity,
      :wind_speed,
      :wind_direction,
      :wind_gust,
      :pressure,
      :rain_1h,
      :rain_24h,
      :rain_since_midnight,
      :manufacturer,
      :equipment_type,
      :course,
      :speed,
      :altitude,
      :addressee,
      :message_text,
      :message_number
    ])
    |> validate_required([
      :base_callsign,
      :data_type,
      :destination,
      :information_field,
      :path,
      :sender,
      :ssid,
      :received_at
    ])
    |> maybe_set_has_position()
  end

  defp maybe_set_has_position(changeset) do
    if (get_field(changeset, :lat) && get_field(changeset, :lon)) ||
         (get_change(changeset, :data_extended) &&
            get_change(changeset, :data_extended).latitude &&
            get_change(changeset, :data_extended).longitude) do
      put_change(changeset, :has_position, true)
    else
      changeset
    end
  end

  # Convert atom data_type to string for storage
  defp normalize_data_type(%{data_type: data_type} = attrs) when is_atom(data_type) do
    %{attrs | data_type: to_string(data_type)}
  end

  defp normalize_data_type(%{"data_type" => data_type} = attrs) when is_atom(data_type) do
    %{attrs | "data_type" => to_string(data_type)}
  end

  # Handle :data_type key access format
  defp normalize_data_type(attrs) when is_map(attrs) do
    if Map.has_key?(attrs, :data_type) and is_atom(attrs.data_type) do
      %{attrs | data_type: to_string(attrs.data_type)}
    else
      attrs
    end
  end

  defp normalize_data_type(attrs), do: attrs

  @doc """
  Extracts additional data from the parsed packet's data_extended field
  and merges it with the packet attributes for storage.
  """
  def extract_additional_data(attrs, raw_packet \\ nil) do
    data_extended = attrs[:data_extended] || attrs["data_extended"] || %{}

    # Start with the base attributes and add the raw packet
    base_attrs = Map.put(attrs, :raw_packet, raw_packet)

    # Extract data based on the type of data_extended
    additional_data =
      case data_extended do
        %{__original_struct__: MicE} = mic_e_map ->
          extract_from_mic_e_map(mic_e_map)

        %{} when is_map(data_extended) ->
          extract_from_map(data_extended)

        %MicE{} = mic_e ->
          extract_from_mic_e(mic_e)

        _ ->
          %{}
      end

    Map.merge(base_attrs, additional_data)
  end

  # Extract data from standard map-based data_extended
  defp extract_from_map(data_extended) do
    %{}
    |> maybe_put(:symbol_code, data_extended[:symbol_code] || data_extended["symbol_code"])
    |> maybe_put(:symbol_table_id, data_extended[:symbol_table_id] || data_extended["symbol_table_id"])
    |> maybe_put(:comment, data_extended[:comment] || data_extended["comment"])
    |> maybe_put(:timestamp, data_extended[:timestamp] || data_extended["timestamp"])
    |> maybe_put(:aprs_messaging, data_extended[:aprs_messaging?] || data_extended["aprs_messaging?"])
    |> maybe_put(:temperature, data_extended[:temperature] || data_extended["temperature"])
    |> maybe_put(:humidity, data_extended[:humidity] || data_extended["humidity"])
    |> maybe_put(:wind_speed, data_extended[:wind_speed] || data_extended["wind_speed"])
    |> maybe_put(:wind_direction, data_extended[:wind_direction] || data_extended["wind_direction"])
    |> maybe_put(:wind_gust, data_extended[:wind_gust] || data_extended["wind_gust"])
    |> maybe_put(:pressure, data_extended[:pressure] || data_extended["pressure"])
    |> maybe_put(:rain_1h, data_extended[:rain_1h] || data_extended["rain_1h"])
    |> maybe_put(:rain_24h, data_extended[:rain_24h] || data_extended["rain_24h"])
    |> maybe_put(:rain_since_midnight, data_extended[:rain_since_midnight] || data_extended["rain_since_midnight"])
    |> maybe_put(:manufacturer, data_extended[:manufacturer] || data_extended["manufacturer"])
    |> maybe_put(:equipment_type, data_extended[:equipment_type] || data_extended["equipment_type"])
    |> maybe_put(:course, data_extended[:course] || data_extended["course"])
    |> maybe_put(:speed, data_extended[:speed] || data_extended["speed"])
    |> maybe_put(:altitude, data_extended[:altitude] || data_extended["altitude"])
    |> maybe_put(:addressee, data_extended[:addressee] || data_extended["addressee"])
    |> maybe_put(:message_text, data_extended[:message_text] || data_extended["message_text"])
    |> maybe_put(:message_number, data_extended[:message_number] || data_extended["message_number"])
    |> extract_weather_data(data_extended)
  end

  # Extract data from MicE packets
  defp extract_from_mic_e(mic_e) do
    %{}
    |> maybe_put(:comment, mic_e.message)
    |> maybe_put(:manufacturer, mic_e.manufacturer)
    |> maybe_put(:equipment_type, mic_e.equipment_type)
    |> maybe_put(:course, mic_e.course)
    |> maybe_put(:speed, mic_e.speed)
    # Default car symbol for MicE
    |> maybe_put(:symbol_code, ">")
    # Primary table
    |> maybe_put(:symbol_table_id, "/")
  end

  # Extract data from converted MicE map (from struct_to_map conversion)
  defp extract_from_mic_e_map(mic_e_map) do
    %{}
    |> maybe_put(:comment, mic_e_map[:message])
    |> maybe_put(:manufacturer, mic_e_map[:manufacturer])
    |> maybe_put(:equipment_type, mic_e_map[:equipment_type])
    |> maybe_put(:course, mic_e_map[:heading])
    |> maybe_put(:speed, mic_e_map[:speed])
    # Default car symbol for MicE
    |> maybe_put(:symbol_code, ">")
    # Primary table
    |> maybe_put(:symbol_table_id, "/")
  end

  # Extract weather data from various formats
  defp extract_weather_data(attrs, data_extended) do
    # Look for weather report in different possible locations
    weather_data =
      data_extended[:weather] || data_extended["weather"] ||
        data_extended[:weather_report] || data_extended["weather_report"]

    case weather_data do
      weather when is_binary(weather) ->
        parse_weather_string(attrs, weather)

      weather when is_map(weather) ->
        Map.merge(attrs, weather)

      _ ->
        attrs
    end
  end

  # Parse weather data from string format (basic implementation)
  defp parse_weather_string(attrs, weather_string) do
    # This is a simplified parser - a full implementation would handle
    # the complete APRS weather format specification
    attrs
    |> maybe_extract_weather_field(weather_string, ~r/(\d{3})\/(\d{3})/, [:wind_direction, :wind_speed])
    |> maybe_extract_weather_field(weather_string, ~r/t(\d{3})/, [:temperature])
    |> maybe_extract_weather_field(weather_string, ~r/h(\d{2})/, [:humidity])
    |> maybe_extract_weather_field(weather_string, ~r/b(\d{5})/, [:pressure])
  end

  # Helper to extract weather fields using regex
  defp maybe_extract_weather_field(attrs, weather_string, regex, keys) do
    case Regex.run(regex, weather_string) do
      [_full | matches] ->
        keys
        |> Enum.zip(matches)
        |> Enum.reduce(attrs, fn {key, value}, acc ->
          case Integer.parse(value) do
            {int_val, _} -> Map.put(acc, key, int_val)
            :error -> acc
          end
        end)

      _ ->
        attrs
    end
  end

  # Helper to put a value only if it's not nil
  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, _key, ""), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)
end

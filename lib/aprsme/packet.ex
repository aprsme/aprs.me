defmodule Aprsme.Packet do
  @moduledoc false
  use Aprsme.Schema

  import Ecto.Changeset

  alias Aprs.Types.MicE
  alias Aprsme.DataExtended

  schema "packets" do
    field(:base_callsign, :string)
    field(:data_type, :string)
    field(:destination, :string)
    field(:information_field, :string)
    field(:path, :string)
    field(:sender, :string)
    field(:ssid, :string)
    field(:received_at, :utc_datetime)
    field(:region, :string)
    field(:lat, :decimal)
    field(:lon, :decimal)
    field(:location, Geo.PostGIS.Geometry)
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
    field(:snow, :float)

    # Equipment/status information
    field(:luminosity, :integer)
    field(:manufacturer, :string)
    field(:equipment_type, :string)
    field(:course, :integer)
    field(:speed, :float)
    field(:altitude, :float)

    # PHG (Power-Height-Gain) fields
    field(:phg_power, :integer)
    field(:phg_height, :integer)
    field(:phg_gain, :integer)
    field(:phg_directivity, :integer)

    # Message-specific fields
    field(:addressee, :string)
    field(:message_text, :string)
    field(:message_number, :string)

    field(:device_identifier, :string)

    embeds_one(:data_extended, DataExtended)

    timestamps(type: :utc_datetime)
  end

  @type t :: %__MODULE__{}

  @doc false
  @spec changeset(Aprsme.Packet.t(), map()) :: Ecto.Changeset.t()
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
      :location,
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
      :snow,
      :luminosity,
      :manufacturer,
      :equipment_type,
      :course,
      :speed,
      :altitude,
      :phg_power,
      :phg_height,
      :phg_gain,
      :phg_directivity,
      :addressee,
      :message_text,
      :message_number,
      :device_identifier
    ])
    |> validate_required([
      :base_callsign,
      :data_type,
      :destination,
      :sender,
      :ssid,
      :received_at
    ])
    |> maybe_set_location_and_position()
  end

  defp maybe_set_location_and_position(changeset) do
    changeset
    |> maybe_create_geometry_from_lat_lon()
    |> maybe_set_has_position()
  end

  @spec maybe_create_geometry_from_lat_lon(Ecto.Changeset.t()) :: Ecto.Changeset.t()
  defp maybe_create_geometry_from_lat_lon(changeset) do
    lat = get_field(changeset, :lat) || get_change(changeset, :lat)
    lon = get_field(changeset, :lon) || get_change(changeset, :lon)

    # Also check data_extended for coordinates
    {lat, lon} = extract_coordinates_from_changeset(changeset, {lat, lon})

    create_geometry_from_coordinates(changeset, lat, lon)
  end

  defp extract_coordinates_from_changeset(changeset, {nil, nil}) do
    data_extended = get_change(changeset, :data_extended)
    extract_coordinates_from_data_extended(data_extended)
  end

  defp extract_coordinates_from_changeset(_changeset, coords), do: coords

  defp extract_coordinates_from_data_extended(nil), do: {nil, nil}

  defp extract_coordinates_from_data_extended(data_extended)
       when is_map(data_extended) and not is_struct(data_extended) do
    {data_extended[:latitude], data_extended[:longitude]}
  end

  defp extract_coordinates_from_data_extended(_), do: {nil, nil}

  defp create_geometry_from_coordinates(changeset, lat, lon) do
    if valid_coordinates?(lat, lon) do
      create_and_set_location(changeset, lat, lon)
    else
      changeset
    end
  end

  defp create_and_set_location(changeset, lat, lon) do
    case create_point(lat, lon) do
      nil -> changeset
      location -> put_change(changeset, :location, location)
    end
  rescue
    error ->
      require Logger

      Logger.error("Failed to create geometry for lat=#{lat}, lon=#{lon}: #{inspect(error)}")
      changeset
  end

  defp maybe_set_has_position(changeset) do
    location = get_field(changeset, :location) || get_change(changeset, :location)

    case location do
      nil -> check_legacy_coordinates(changeset)
      _location -> put_change(changeset, :has_position, true)
    end
  end

  defp check_legacy_coordinates(changeset) do
    lat = get_field(changeset, :lat) || get_change(changeset, :lat)
    lon = get_field(changeset, :lon) || get_change(changeset, :lon)

    if valid_coordinates?(lat, lon) do
      put_change(changeset, :has_position, true)
    else
      changeset
    end
  end

  defp valid_coordinates?(lat, lon) do
    lat = normalize_coordinate(lat)
    lon = normalize_coordinate(lon)

    is_number(lat) && is_number(lon) &&
      lat >= -90 && lat <= 90 &&
      lon >= -180 && lon <= 180
  end

  defp normalize_coordinate(%Decimal{} = decimal), do: Decimal.to_float(decimal)
  defp normalize_coordinate(coord), do: coord

  # Convert atom data_type to string for storage
  defp normalize_data_type(%{data_type: data_type} = attrs) when is_atom(data_type) do
    %{attrs | data_type: to_string(data_type)}
  end

  defp normalize_data_type(%{"data_type" => data_type} = attrs) when is_atom(data_type) do
    %{attrs | "data_type" => to_string(data_type)}
  end

  defp normalize_data_type(attrs) when is_map(attrs) do
    case {Map.has_key?(attrs, :data_type), Map.get(attrs, :data_type)} do
      {true, data_type} when is_atom(data_type) ->
        %{attrs | data_type: to_string(data_type)}

      _ ->
        attrs
    end
  end

  defp normalize_data_type(attrs), do: attrs

  @doc """
  Extracts additional data from the raw packet and data_extended structure
  and merges it with the packet attributes for storage.
  """
  @spec extract_additional_data(map(), String.t() | nil) :: map()
  def extract_additional_data(attrs, raw_packet \\ nil) do
    data_extended = attrs[:data_extended] || attrs["data_extended"] || %{}

    # Start with the base attributes and add the raw packet
    base_attrs = Map.put(attrs, :raw_packet, raw_packet)

    # Remove raw_weather_data from base attributes
    base_attrs = Map.delete(base_attrs, :raw_weather_data)
    base_attrs = Map.delete(base_attrs, "raw_weather_data")

    # Check if symbol data exists at the top level of attrs (from APRS parser)
    # and preserve it if not already in base_attrs
    base_attrs =
      base_attrs
      |> maybe_put(:symbol_code, attrs[:symbol_code] || attrs["symbol_code"])
      |> maybe_put(:symbol_table_id, attrs[:symbol_table_id] || attrs["symbol_table_id"])

    # Extract data based on the type of data_extended
    additional_data =
      case data_extended do
        %MicE{} = mic_e ->
          extract_from_mic_e(mic_e)

        %{__original_struct__: MicE} = mic_e_map ->
          extract_from_mic_e_map(mic_e_map)

        # Handle ParseError structs gracefully
        %{__struct__: Aprs.Types.ParseError} ->
          %{}

        %{} when is_map(data_extended) ->
          extract_from_map(data_extended)

        _ ->
          %{}
      end

    Map.merge(base_attrs, additional_data)
  end

  # Extract data from standard map-based data_extended
  # Convert struct to map if possible, otherwise return empty map
  defp extract_from_map(data_extended) when is_struct(data_extended) do
    data_extended
    |> Map.from_struct()
    |> extract_from_map()
  rescue
    _ -> %{}
  end

  defp extract_from_map(data_extended) when is_map(data_extended) do
    # Remove raw_weather_data before processing
    data_extended = Map.delete(data_extended, :raw_weather_data)
    data_extended = Map.delete(data_extended, "raw_weather_data")

    # Handle nested data_extended structures
    nested_data_extended = data_extended[:data_extended] || data_extended["data_extended"]

    # Merge data from both levels
    combined_data =
      if nested_data_extended do
        Map.merge(data_extended, nested_data_extended)
      else
        data_extended
      end

    result =
      %{}
      |> put_symbol_fields(combined_data)
      |> extract_weather_data(combined_data)
      |> put_weather_fields(combined_data)
      |> put_equipment_fields(combined_data)
      |> put_message_fields(combined_data)

    # Don't override data_type - trust the APRS parser's determination
    # The parser already correctly identifies weather packets by:
    # 1. Data type indicator (e.g., "_" for weather)
    # 2. Symbol (e.g., "/_" for weather station)
    # 3. Actual weather data patterns in the content
    result
  end

  defp put_symbol_fields(map, data_extended) do
    # First try to get symbol data from the data_extended map
    symbol_code = data_extended[:symbol_code] || data_extended["symbol_code"]
    symbol_table_id = data_extended[:symbol_table_id] || data_extended["symbol_table_id"]

    map
    |> maybe_put(:symbol_code, symbol_code)
    |> maybe_put(:symbol_table_id, symbol_table_id)
    |> maybe_put(:comment, data_extended[:comment] || data_extended["comment"])
    |> maybe_put(:timestamp, data_extended[:timestamp] || data_extended["timestamp"])
    |> maybe_put(
      :aprs_messaging,
      data_extended[:aprs_messaging?] || data_extended["aprs_messaging?"]
    )
  end

  defp put_weather_fields(map, data_extended) do
    Enum.reduce(Aprsme.EncodingUtils.weather_fields(), map, fn field, acc ->
      value = data_extended[field] || data_extended[to_string(field)]
      maybe_put(acc, field, value)
    end)
  end

  defp put_equipment_fields(map, data_extended) do
    map
    |> maybe_put(:manufacturer, data_extended[:manufacturer] || data_extended["manufacturer"])
    |> maybe_put(
      :equipment_type,
      data_extended[:equipment_type] || data_extended["equipment_type"]
    )
    |> maybe_put(:course, data_extended[:course] || data_extended["course"])
    |> maybe_put(:speed, data_extended[:speed] || data_extended["speed"])
    |> maybe_put(:altitude, data_extended[:altitude] || data_extended["altitude"])
    |> put_phg_fields(data_extended)
  end

  defp put_phg_fields(map, data_extended) do
    phg = data_extended[:phg] || data_extended["phg"]

    if phg && is_map(phg) do
      map
      |> maybe_put(:phg_power, phg[:power] || phg["power"])
      |> maybe_put(:phg_height, phg[:height] || phg["height"])
      |> maybe_put(:phg_gain, phg[:gain] || phg["gain"])
      |> maybe_put(:phg_directivity, phg[:directivity] || phg["directivity"])
    else
      map
    end
  end

  defp put_message_fields(map, data_extended) do
    map
    |> maybe_put(:addressee, data_extended[:addressee] || data_extended["addressee"])
    |> maybe_put(:message_text, data_extended[:message_text] || data_extended["message_text"])
    |> maybe_put(
      :message_number,
      data_extended[:message_number] || data_extended["message_number"]
    )
  end

  # Extract data from MicE packets
  defp extract_from_mic_e(mic_e) do
    %{}
    |> maybe_put(:lat, mic_e[:latitude])
    |> maybe_put(:lon, mic_e[:longitude])
    |> maybe_put(:comment, mic_e.message)
    |> maybe_put(:manufacturer, mic_e.manufacturer)
    |> maybe_put(:course, mic_e.heading)
    |> maybe_put(:speed, mic_e.speed)
    |> maybe_put(:symbol_code, mic_e.symbol_code)
    |> maybe_put(:symbol_table_id, mic_e.symbol_table_id)
  end

  # Extract data from converted MicE map (from struct_to_map conversion)
  defp extract_from_mic_e_map(mic_e_map) do
    %{}
    |> maybe_put(:lat, mic_e_map[:latitude])
    |> maybe_put(:lon, mic_e_map[:longitude])
    |> maybe_put(:comment, mic_e_map[:message])
    |> maybe_put(:manufacturer, mic_e_map[:manufacturer])
    |> maybe_put(:course, mic_e_map[:heading])
    |> maybe_put(:speed, mic_e_map[:speed])
    # Use symbol data from MicE if available, otherwise use default car symbol
    |> maybe_put(:symbol_code, mic_e_map[:symbol_code] || mic_e_map["symbol_code"] || ">")
    |> maybe_put(:symbol_table_id, mic_e_map[:symbol_table_id] || mic_e_map["symbol_table_id"] || "/")
  end

  # Extract weather data from various formats
  defp extract_weather_data(attrs, data_extended) do
    weather_data = find_weather_data(data_extended)
    process_weather_data(attrs, weather_data)
  end

  defp find_weather_data(data_extended) do
    data_extended[:weather] || data_extended["weather"] ||
      data_extended[:weather_report] || data_extended["weather_report"] ||
      data_extended[:raw_weather_data] || data_extended["raw_weather_data"]
  end

  defp process_weather_data(attrs, weather_data) do
    case weather_data do
      weather when is_binary(weather) ->
        process_binary_weather_data(attrs, weather)

      weather when is_map(weather) ->
        process_map_weather_data(attrs, weather)

      _ ->
        attrs
    end
  end

  defp process_binary_weather_data(attrs, _weather) do
    # If you have a weather parsing function, call it here
    attrs
  end

  defp process_map_weather_data(attrs, weather) do
    weather = Map.drop(weather, [:raw_weather_data, "raw_weather_data"])

    # Only merge weather data, don't override data_type
    Map.merge(attrs, weather)
  end

  # Helper to put a value only if it's not nil
  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, _key, ""), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  # @doc """
  # Spatial query functions for efficient location-based searches
  # """
  # Temporarily commented out until PostGIS is properly configured

  # @doc """
  # Find packets within a given radius (in meters) of a point.
  # """
  # def within_radius(query \\ __MODULE__, lat, lon, radius_meters) do
  #   point = %Geo.Point{coordinates: {lon, lat}, srid: 4326}

  #   from p in query,
  #     where: st_dwithin_in_meters(p.location, ^point, ^radius_meters),
  #     where: not is_nil(p.location)
  # end

  # @doc """
  # Find packets within a bounding box defined by southwest and northeast corners.
  # """
  # def within_bbox(query \\ __MODULE__, sw_lat, sw_lon, ne_lat, ne_lon) do
  #   # Create a polygon representing the bounding box
  #   bbox = %Geo.Polygon{
  #     coordinates: [[
  #       {sw_lon, sw_lat},
  #       {ne_lon, sw_lat},
  #       {ne_lon, ne_lat},
  #       {sw_lon, ne_lat},
  #       {sw_lon, sw_lat}
  #     ]],
  #     srid: 4326
  #   }

  #   from p in query,
  #     where: st_within(p.location, ^bbox),
  #     where: not is_nil(p.location)
  # end

  # @doc """
  # Find packets ordered by distance from a given point.
  # """
  # def nearest_to(query \\ __MODULE__, lat, lon, limit \\ 100) do
  #   point = %Geo.Point{coordinates: {lon, lat}, srid: 4326}

  #   from p in query,
  #     where: not is_nil(p.location),
  #     order_by: st_distance(p.location, ^point),
  #     limit: ^limit,
  #     select: %{p | distance: st_distance_in_meters(p.location, ^point)}
  # end

  # @doc """
  # Find recent packets with location data within the last N hours.
  # """
  # def recent_with_location(query \\ __MODULE__, hours_back \\ 24) do
  #   cutoff_time = DateTime.utc_now() |> DateTime.add(-hours_back, :hour)

  #   from p in query,
  #     where: p.has_position == true,
  #     where: not is_nil(p.location),
  #     where: p.received_at > ^cutoff_time,
  #     order_by: [desc: p.received_at]
  # end

  # @doc """
  # Get statistics for packets in a geographic area.
  # """
  # def location_stats(query \\ __MODULE__, lat, lon, radius_meters) do
  #   point = %Geo.Point{coordinates: {lon, lat}, srid: 4326}

  #   from p in query,
  #     where: st_dwithin_in_meters(p.location, ^point, ^radius_meters),
  #     where: not is_nil(p.location),
  #     group_by: p.base_callsign,
  #     select: %{
  #       callsign: p.base_callsign,
  #       packet_count: count(p.id),
  #       latest_position: max(p.received_at),
  #       avg_lat: avg(fragment("ST_Y(?)", p.location)),
  #       avg_lon: avg(fragment("ST_X(?)", p.location))
  #     }
  # end

  @doc """
  Create a geometry point from lat/lon coordinates.
  """
  @spec create_point(number() | nil, number() | nil) :: Geo.Point.t() | nil
  def create_point(lat, lon)
      when (is_number(lat) or is_struct(lat, Decimal)) and (is_number(lon) or is_struct(lon, Decimal)) do
    lat = normalize_coordinate(lat)
    lon = normalize_coordinate(lon)

    if valid_coordinates?(lat, lon) do
      %Geo.Point{coordinates: {lon, lat}, srid: 4326}
    end
  end

  def create_point(_, _), do: nil

  @doc """
  Extract lat/lon from a PostGIS geometry point.
  """
  @spec extract_coordinates(Geo.Point.t() | any()) :: {number() | nil, number() | nil}
  def extract_coordinates(%Geo.Point{coordinates: {lon, lat}}), do: {lat, lon}
  def extract_coordinates(_), do: {nil, nil}

  @doc """
  Get latitude from a packet's location geometry.
  """
  @spec lat(Aprsme.Packet.t()) :: number() | nil
  def lat(%__MODULE__{location: %Geo.Point{coordinates: {_lon, lat}}}), do: lat
  def lat(_), do: nil

  @doc """
  Get longitude from a packet's location geometry.
  """
  @spec lon(Aprsme.Packet.t()) :: number() | nil
  def lon(%__MODULE__{location: %Geo.Point{coordinates: {lon, _lat}}}), do: lon
  def lon(_), do: nil

  # @doc """
  # Calculate distance between two packets in meters.
  # """
  # def distance_between(%__MODULE__{location: %Geo.Point{} = p1}, %__MODULE__{location: %Geo.Point{} = p2}) do
  #   Repo.one(
  #     from p in "packets",
  #       select: fragment("ST_Distance_Sphere(?, ?)", ^p1, ^p2),
  #       limit: 1
  #   )
  # end

  # def distance_between(_, _), do: nil
end

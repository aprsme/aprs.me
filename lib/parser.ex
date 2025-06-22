# @dialyzer {:nowarn_function, parse_data: 3}
# @dialyzer {:nowarn_function, parse_object: 1}
defmodule Parser do
  @moduledoc """
  Main parsing library
  """
  # import Bitwise
  alias Aprs.Convert
  alias Parser.MicE

  # Simple APRS position parsing to replace parse_aprs_position
  defp parse_aprs_position(lat, lon) do
    # Regex for latitude: 2 deg, 2+ min, 1 dir (N/S)
    # Regex for longitude: 3 deg, 2+ min, 1 dir (E/W)
    lat_re = ~r/^(\d{2})(\d{2}\.\d+)([NS])$/
    lon_re = ~r/^(\d{3})(\d{2}\.\d+)([EW])$/

    with [_, lat_deg, lat_min, lat_dir] <- Regex.run(lat_re, lat),
         [_, lon_deg, lon_min, lon_dir] <- Regex.run(lon_re, lon) do
      lat_val =
        Decimal.add(Decimal.new(lat_deg), Decimal.div(Decimal.new(lat_min), Decimal.new("60")))

      lon_val =
        Decimal.add(Decimal.new(lon_deg), Decimal.div(Decimal.new(lon_min), Decimal.new("60")))

      lat = if lat_dir == "S", do: Decimal.negate(lat_val), else: lat_val
      lon = if lon_dir == "W", do: Decimal.negate(lon_val), else: lon_val
      %{latitude: lat, longitude: lon}
    else
      _ -> %{latitude: nil, longitude: nil}
    end
  end

  @type packet :: %{
          id: String.t(),
          sender: String.t(),
          path: String.t(),
          destination: String.t(),
          information_field: String.t(),
          data_type: atom(),
          base_callsign: String.t(),
          ssid: String.t(),
          data_extended: map() | nil,
          received_at: DateTime.t()
        }

  @type parse_result :: {:ok, packet()} | {:error, atom() | String.t()}

  @type position_ambiguity :: 0..4

  @spec parse(String.t()) :: parse_result()
  def parse(message) when is_binary(message) do
    do_parse(message)
  rescue
    _ ->
      {:error, :invalid_packet}
  end

  def parse(_), do: {:error, :invalid_packet}

  @spec do_parse(String.t()) :: parse_result()
  defp do_parse(message) do
    with {:ok, [sender, path, data]} <- split_packet(message),
         {:ok, callsign_parts} <- parse_callsign(sender),
         {:ok, data_type} <- parse_datatype_safe(data),
         {:ok, [destination, path]} <- split_path(path),
         :ok <- validate_path(path) do
      data_trimmed = String.trim(data)
      data_without_type = String.slice(data_trimmed, 1..-1//1)
      data_extended = parse_data(data_type, destination, data_without_type)

      base_callsign = List.first(callsign_parts)

      ssid =
        case List.last(callsign_parts) do
          nil ->
            nil

          s when is_binary(s) ->
            s

          i when is_integer(i) ->
            to_string(i)

          _ ->
            nil
        end

      {:ok,
       %{
         id: 16 |> :crypto.strong_rand_bytes() |> Base.encode16(case: :lower),
         sender: sender,
         path: path,
         destination: destination,
         information_field: data_trimmed,
         data_type: data_type,
         base_callsign: base_callsign,
         ssid: ssid,
         data_extended: data_extended,
         received_at: DateTime.truncate(DateTime.utc_now(), :microsecond)
       }}
    end
  rescue
    _ ->
      {:error, :invalid_packet}
  end

  # Validate path for too many components
  def validate_path(path) when is_binary(path) and path != "" do
    if length(String.split(path, ",")) > 8 do
      {:error, "Too many path components"}
    else
      :ok
    end
  end

  def validate_path(_), do: :ok

  # Safely split packet into components
  @spec split_packet(String.t()) :: {:ok, [String.t()]} | {:error, String.t()}
  def split_packet(message) do
    split_packet_parts(String.split(message, [">", ":"], parts: 3))
  end

  @spec split_packet_parts([String.t()]) :: {:ok, [String.t()]} | {:error, String.t()}
  defp split_packet_parts([sender, path, data]) when byte_size(sender) > 0 and byte_size(path) > 0 do
    {:ok, [sender, path, data]}
  end

  @spec split_packet_parts(list()) :: {:error, String.t()}
  defp split_packet_parts(_), do: {:error, "Invalid packet format"}

  # Safely split path into destination and digipeater path
  @spec split_path(String.t()) :: {:ok, [String.t()]} | {:error, String.t()}
  def split_path(path) when is_binary(path) do
    split = String.split(path, ",", parts: 2)
    split_path_parts(split)
  end

  defp split_path_parts([destination, digi_path]), do: {:ok, [destination, digi_path]}
  defp split_path_parts([destination]), do: {:ok, [destination, ""]}
  defp split_path_parts(_), do: {:error, "Invalid path format"}

  # Safe version of parse_datatype that returns {:ok, type} or {:error, reason}
  @spec parse_datatype_safe(String.t()) :: {:ok, atom()} | {:error, String.t()}
  def parse_datatype_safe(""), do: {:error, "Empty data"}
  def parse_datatype_safe(data), do: {:ok, parse_datatype(data)}

  @spec parse_callsign(String.t()) :: {:ok, [String.t()]} | {:error, String.t()}
  def parse_callsign(callsign) do
    case Parser.AX25.parse_callsign(callsign) do
      {:ok, {base, ssid}} -> {:ok, [base, ssid]}
      {:error, reason} -> {:error, reason}
    end
  end

  # One of the nutty exceptions in the APRS protocol has to do with this
  # data type indicator. It's usually the first character of the message.
  # However, in some rare cases, the ! indicator can be anywhere in the
  # first 40 characters of the message. I'm not going to deal with that
  # weird case right now. It seems like its for a specific type of old
  # TNC hardware that probably doesn't even exist anymore.
  @spec parse_datatype(String.t()) :: atom()
  def parse_datatype(<<":", _::binary>>), do: :message
  def parse_datatype(<<">", _::binary>>), do: :status
  def parse_datatype("!" <> _), do: :position
  def parse_datatype("/" <> _), do: :timestamped_position
  def parse_datatype("=" <> _), do: :position_with_message
  def parse_datatype("@" <> _), do: :timestamped_position_with_message
  def parse_datatype(";" <> _), do: :object
  def parse_datatype("`" <> _), do: :mic_e_old
  def parse_datatype("'" <> _), do: :mic_e_old
  def parse_datatype("_" <> _), do: :weather
  def parse_datatype("T" <> _), do: :telemetry
  def parse_datatype("$" <> _), do: :raw_gps_ultimeter
  def parse_datatype("<" <> _), do: :station_capabilities
  def parse_datatype("?" <> _), do: :query
  def parse_datatype("{" <> _), do: :user_defined
  def parse_datatype("}" <> _), do: :third_party_traffic
  def parse_datatype("%" <> _), do: :item
  def parse_datatype(")" <> _), do: :item
  def parse_datatype("*" <> _), do: :peet_logging
  def parse_datatype("," <> _), do: :invalid_test_data
  def parse_datatype("#DFS" <> _), do: :df_report
  def parse_datatype("#PHG" <> _), do: :phg_data
  def parse_datatype("#" <> _), do: :phg_data
  def parse_datatype(_), do: :unknown_datatype

  @spec parse_data(atom(), String.t(), String.t()) :: map() | nil
  def parse_data(:mic_e, destination, data), do: MicE.parse(data, destination)
  def parse_data(:mic_e_old, destination, data), do: MicE.parse(data, destination)

  def parse_data(:position, destination, <<"!", rest::binary>>) do
    parse_data(:position, destination, rest)
  end

  def parse_data(:position, _destination, <<"/", _::binary>> = data) do
    result = parse_position_without_timestamp(data)
    if result.data_type == :malformed_position, do: result, else: %{result | data_type: :position}
  end

  def parse_data(:position, _destination, data) do
    result = parse_position_without_timestamp(data)
    if result.data_type == :malformed_position, do: result, else: %{result | data_type: :position}
  end

  def parse_data(:position_with_message, _destination, data) do
    result = parse_position_with_message_without_timestamp(data)
    %{result | data_type: :position}
  end

  def parse_data(:timestamped_position, _destination, data) do
    parse_position_with_timestamp(false, data, :timestamped_position)
  end

  def parse_data(:timestamped_position_with_message, _destination, data) do
    case data do
      <<time::binary-size(7), latitude::binary-size(8), sym_table_id::binary-size(1), longitude::binary-size(9),
        symbol_code::binary-size(1), rest::binary>> ->
        weather_start = String.starts_with?(rest, "_")

        if weather_start do
          result =
            Parser.parse_position_with_datetime_and_weather(
              true,
              time,
              latitude,
              sym_table_id,
              longitude,
              symbol_code,
              rest
            )

          add_has_location(result)
        else
          result = parse_position_with_timestamp(true, data, :timestamped_position_with_message)
          add_has_location(result)
        end

      _ ->
        result = parse_position_with_timestamp(true, data, :timestamped_position_with_message)
        add_has_location(result)
    end
  end

  def parse_data(:message, _destination, data) do
    case Regex.run(~r/^:([^:]+):(.+?)(\{(\d+)\})?$/s, data) do
      [_, addressee, message_text, _full_ack, message_number] ->
        %{
          data_type: :message,
          addressee: String.trim(addressee),
          message_text: String.trim(message_text),
          message_number: message_number
        }

      [_, addressee, message_text] ->
        %{
          data_type: :message,
          addressee: String.trim(addressee),
          message_text: String.trim(message_text)
        }

      _ ->
        nil
    end
  end

  def parse_data(:status, _destination, data), do: Parser.Status.parse(data)
  def parse_data(:object, _destination, data), do: Parser.Object.parse(data)
  def parse_data(:item, _destination, data), do: Parser.Item.parse(data)
  def parse_data(:weather, _destination, data), do: Parser.Weather.parse(data)
  def parse_data(:telemetry, _destination, data), do: Parser.Telemetry.parse(data)
  def parse_data(:station_capabilities, _destination, data), do: parse_station_capabilities(data)
  def parse_data(:query, _destination, data), do: parse_query(data)
  def parse_data(:user_defined, _destination, data), do: parse_user_defined(data)
  def parse_data(:third_party_traffic, _destination, data), do: parse_third_party_traffic(data)
  def parse_data(:peet_logging, _destination, data), do: Parser.Helpers.parse_peet_logging(data)

  def parse_data(:invalid_test_data, _destination, data), do: Parser.Helpers.parse_invalid_test_data(data)

  def parse_data(:raw_gps_ultimeter, _destination, data) do
    case Parser.Helpers.parse_nmea_sentence(data) do
      {:error, error} ->
        %{
          data_type: :raw_gps_ultimeter,
          error: error,
          nmea_type: nil,
          raw_data: data,
          latitude: nil,
          longitude: nil
        }
    end
  end

  def parse_data(:df_report, _destination, data) do
    if String.starts_with?(data, "DFS") and byte_size(data) >= 7 do
      <<"DFS", s, h, g, d, rest::binary>> = data

      %{
        df_strength: Parser.Helpers.parse_df_strength(s),
        height: Parser.Helpers.parse_phg_height(h),
        gain: Parser.Helpers.parse_phg_gain(g),
        directivity: Parser.Helpers.parse_phg_directivity(d),
        comment: rest,
        data_type: :df_report
      }
    else
      %{
        df_data: data,
        data_type: :df_report
      }
    end
  end

  def parse_data(:phg_data, _destination, data), do: parse_phg_data(data)
  def parse_data(_type, _destination, _data), do: nil

  defp add_has_location(result) do
    Map.put(
      result,
      :has_location,
      (is_number(result[:latitude]) or is_struct(result[:latitude], Decimal)) and
        (is_number(result[:longitude]) or is_struct(result[:longitude], Decimal))
    )
  end

  @spec parse_position_with_datetime_and_weather(
          boolean(),
          binary(),
          binary(),
          binary(),
          binary(),
          binary(),
          binary()
        ) :: map()
  def parse_position_with_datetime_and_weather(
        aprs_messaging?,
        time,
        latitude,
        sym_table_id,
        longitude,
        symbol_code,
        weather_report
      ) do
    pos = parse_aprs_position(latitude, longitude)
    weather_data = parse_weather_data(weather_report)

    %{
      latitude: pos.latitude,
      longitude: pos.longitude,
      timestamp: time,
      symbol_table_id: sym_table_id,
      symbol_code: symbol_code,
      weather: weather_data,
      data_type: :position_with_datetime_and_weather,
      aprs_messaging?: aprs_messaging?
    }
  end

  @spec decode_compressed_position(binary()) :: map()
  def decode_compressed_position(
        <<"/", latitude::binary-size(4), longitude::binary-size(4), symbol_code::binary-size(1), _cs::binary-size(2),
          _compression_type::binary-size(2), _rest::binary>>
      ) do
    lat = Parser.Helpers.convert_to_base91(latitude)
    lon = Parser.Helpers.convert_to_base91(longitude)

    %{
      latitude: lat,
      longitude: lon,
      symbol_code: symbol_code
    }
  end

  @spec convert_to_base91(binary()) :: integer()
  def convert_to_base91(<<value::binary-size(4)>>) do
    [v1, v2, v3, v4] = to_charlist(value)
    (v1 - 33) * 91 * 91 * 91 + (v2 - 33) * 91 * 91 + (v3 - 33) * 91 + v4
  end

  # Helper to extract course and speed from comment field (e.g., "/123/045" or "123/045")
  @spec extract_course_and_speed(String.t()) :: {integer() | nil, float() | nil}
  defp extract_course_and_speed(comment) do
    # Match "/123/045" or "123/045" at the start of the comment
    case Regex.run(~r"/?(\d{3})/(\d{3})", comment) do
      [_, course, speed] -> {String.to_integer(course), String.to_integer(speed) * 1.0}
      _ -> {nil, nil}
    end
  end

  # Patch parse_position_without_timestamp to include course/speed
  @spec parse_position_without_timestamp(String.t()) :: map()
  def parse_position_without_timestamp(position_data) do
    case position_data do
      <<latitude::binary-size(8), sym_table_id::binary-size(1), longitude::binary-size(9), symbol_code::binary-size(1),
        comment::binary>> ->
        parse_position_uncompressed(latitude, sym_table_id, longitude, symbol_code, comment)

      <<latitude::binary-size(8), sym_table_id::binary-size(1), longitude::binary-size(9)>> ->
        parse_position_short_uncompressed(latitude, sym_table_id, longitude)

      <<"/", latitude_compressed::binary-size(4), longitude_compressed::binary-size(4), symbol_code::binary-size(1),
        cs::binary-size(2), compression_type::binary-size(1), comment::binary>> ->
        parse_position_compressed(
          latitude_compressed,
          longitude_compressed,
          symbol_code,
          cs,
          compression_type,
          comment
        )

      _ ->
        parse_position_malformed(position_data)
    end
  end

  defp parse_position_uncompressed(latitude, sym_table_id, longitude, symbol_code, comment) do
    %{latitude: lat, longitude: lon} = parse_aprs_position(latitude, longitude)
    ambiguity = Parser.Helpers.calculate_position_ambiguity(latitude, longitude)
    dao_data = parse_dao_extension(comment)
    {course, speed} = extract_course_and_speed(comment)

    has_position =
      (is_number(lat) or is_struct(lat, Decimal)) and (is_number(lon) or is_struct(lon, Decimal))

    base_map = %{
      latitude: lat,
      longitude: lon,
      timestamp: nil,
      symbol_table_id: sym_table_id,
      symbol_code: symbol_code,
      comment: comment,
      data_type: :position,
      aprs_messaging?: false,
      compressed?: false,
      position_ambiguity: ambiguity,
      dao: dao_data,
      course: course,
      speed: speed,
      has_position: has_position
    }

    if sym_table_id == "/" and symbol_code == "_" do
      weather_map = Parser.Weather.parse_weather_data(comment)
      Map.merge(base_map, weather_map)
    else
      base_map
    end
  end

  defp parse_position_short_uncompressed(latitude, sym_table_id, longitude) do
    %{latitude: lat, longitude: lon} = parse_aprs_position(latitude, longitude)
    ambiguity = Parser.Helpers.calculate_position_ambiguity(latitude, longitude)

    has_position =
      (is_number(lat) or is_struct(lat, Decimal)) and (is_number(lon) or is_struct(lon, Decimal))

    %{
      latitude: lat,
      longitude: lon,
      timestamp: nil,
      symbol_table_id: sym_table_id,
      symbol_code: "_",
      data_type: :position,
      aprs_messaging?: false,
      compressed?: false,
      position_ambiguity: ambiguity,
      dao: nil,
      course: nil,
      speed: nil,
      has_position: has_position
    }
  end

  defp parse_position_compressed(latitude_compressed, longitude_compressed, symbol_code, cs, compression_type, comment) do
    converted_lat = Parser.Helpers.convert_compressed_lat(latitude_compressed)
    converted_lon = Parser.Helpers.convert_compressed_lon(longitude_compressed)
    compressed_cs = Parser.Helpers.convert_compressed_cs(cs)
    ambiguity = Parser.Helpers.calculate_compressed_ambiguity(compression_type)

    has_position =
      (is_number(converted_lat) or is_struct(converted_lat, Decimal)) and
        (is_number(converted_lon) or is_struct(converted_lon, Decimal))

    base_data = %{
      latitude: converted_lat,
      longitude: converted_lon,
      symbol_table_id: "/",
      symbol_code: symbol_code,
      comment: comment,
      position_format: :compressed,
      compression_type: compression_type,
      data_type: :position,
      compressed?: true,
      position_ambiguity: ambiguity,
      dao: nil,
      has_position: has_position
    }

    Map.merge(base_data, compressed_cs)
  rescue
    _e ->
      %{
        latitude: nil,
        longitude: nil,
        symbol_table_id: "/",
        symbol_code: symbol_code,
        comment: comment,
        position_format: :compressed,
        compression_type: compression_type,
        data_type: :position,
        compressed?: true,
        position_ambiguity: Parser.Helpers.calculate_compressed_ambiguity(compression_type),
        dao: nil,
        course: nil,
        speed: nil,
        has_position: false
      }
  end

  defp parse_position_malformed(position_data) do
    %{
      latitude: nil,
      longitude: nil,
      timestamp: nil,
      symbol_table_id: nil,
      symbol_code: nil,
      data_type: :malformed_position,
      aprs_messaging?: false,
      compressed?: false,
      comment: String.trim(position_data),
      dao: nil,
      course: nil,
      speed: nil,
      has_position: false
    }
  end

  # Patch parse_position_with_message_without_timestamp to propagate course/speed
  @spec parse_position_with_message_without_timestamp(String.t()) :: map()
  def parse_position_with_message_without_timestamp(position_data) do
    result = parse_position_without_timestamp(position_data)
    Map.put(result, :aprs_messaging?, true)
  end

  # Patch parse_position_with_timestamp to extract course/speed from comment
  @spec parse_position_with_timestamp(boolean(), binary(), atom()) :: map()
  def parse_position_with_timestamp(
        aprs_messaging?,
        <<time::binary-size(7), latitude::binary-size(8), sym_table_id::binary-size(1), longitude::binary-size(9),
          symbol_code::binary-size(1), comment::binary>>,
        _data_type
      ) do
    case Parser.Helpers.validate_position_data(latitude, longitude) do
      {:ok, {lat, lon}} ->
        position = parse_aprs_position(latitude, longitude)
        {course, speed} = extract_course_and_speed(comment)

        %{
          latitude: lat,
          longitude: lon,
          position: position,
          time: Parser.Helpers.validate_timestamp(time),
          symbol_table_id: sym_table_id,
          symbol_code: symbol_code,
          comment: comment,
          data_type: :position,
          aprs_messaging?: aprs_messaging?,
          compressed?: false,
          course: course,
          speed: speed
        }

      _ ->
        # Fallback: try to extract lat/lon using regex if binary pattern match fails
        regex =
          ~r/^(?<time>\w{7})(?<lat>\d{4,5}\.\d+[NS])(?<sym_table>.)(?<lon>\d{5,6}\.\d+[EW])(?<sym_code>.)(?<comment>.*)$/

        case Regex.named_captures(
               regex,
               time <> latitude <> sym_table_id <> longitude <> symbol_code <> comment
             ) do
          %{
            "lat" => lat,
            "lon" => lon,
            "time" => time,
            "sym_table" => sym_table,
            "sym_code" => sym_code,
            "comment" => comment
          } ->
            pos = parse_aprs_position(lat, lon)

            %{
              latitude: pos.latitude,
              longitude: pos.longitude,
              time: time,
              symbol_table_id: sym_table,
              symbol_code: sym_code,
              comment: comment,
              data_type: :position,
              aprs_messaging?: aprs_messaging?,
              compressed?: false
            }

          _ ->
            %{
              data_type: :timestamped_position_error,
              error: "Invalid timestamped position format",
              raw_data: time <> latitude <> sym_table_id <> longitude <> symbol_code <> comment
            }
        end
    end
  end

  def parse_position_with_timestamp(aprs_messaging?, data, _data_type) do
    # Fallback: try to extract lat/lon using regex if binary pattern match fails
    regex =
      ~r/^(?<time>\w{7})(?<lat>\d{4,5}\.\d+[NS])(?<sym_table>.)(?<lon>\d{5,6}\.\d+[EW])(?<sym_code>.)(?<comment>.*)$/

    case Regex.named_captures(regex, data) do
      %{
        "lat" => lat,
        "lon" => lon,
        "time" => time,
        "sym_table" => sym_table,
        "sym_code" => sym_code,
        "comment" => comment
      } ->
        pos = parse_aprs_position(lat, lon)

        %{
          latitude: pos.latitude,
          longitude: pos.longitude,
          time: time,
          symbol_table_id: sym_table,
          symbol_code: sym_code,
          comment: comment,
          data_type: :position,
          aprs_messaging?: aprs_messaging?,
          compressed?: false
        }

      _ ->
        %{
          data_type: :timestamped_position_error,
          error: "Invalid timestamped position format",
          raw_data: data
        }
    end
  end

  @spec parse_manufacturer(binary()) :: String.t()
  def parse_manufacturer(symbols) do
    Aprs.DeviceIdentification.identify_device(symbols)
  end

  @spec convert_compressed_lat(binary()) :: float()
  def convert_compressed_lat(lat) do
    [l1, l2, l3, l4] = to_charlist(lat)
    90 - ((l1 - 33) * 91 ** 3 + (l2 - 33) * 91 ** 2 + (l3 - 33) * 91 + l4 - 33) / 380_926
  end

  @spec convert_compressed_lon(binary()) :: float()
  def convert_compressed_lon(lon) do
    [l1, l2, l3, l4] = to_charlist(lon)
    -180 + ((l1 - 33) * 91 ** 3 + (l2 - 33) * 91 ** 2 + (l3 - 33) * 91 + l4 - 33) / 190_463
  end

  @spec convert_compressed_cs(binary()) :: map()
  def convert_compressed_cs(cs) do
    [c, s] = to_charlist(cs)
    c = c - 33
    s = s - 33

    case c do
      x when x in ?!..?z ->
        # compressed speed/course value
        # speed is returned in knots
        %{
          course: s * 4,
          speed: Convert.speed(1.08 ** s - 1, :knots, :mph)
        }

      ?Z ->
        # pre-calculated radio range
        %{
          range: 2 * 1.08 ** s
        }

      _ ->
        %{}
    end
  end

  # Status Report parsing
  def parse_status(<<">", status_text::binary>>) do
    %{
      status_text: status_text,
      data_type: :status
    }
  end

  @spec parse_status(String.t()) :: map()
  def parse_status(data) do
    %{
      status_text: data,
      data_type: :status
    }
  end

  # Object Report parsing
  def parse_object(
        <<";", object_name::binary-size(9), live_killed::binary-size(1), timestamp::binary-size(7), rest::binary>>
      ) do
    position_data =
      case rest do
        # Uncompressed position format
        <<latitude::binary-size(8), sym_table_id::binary-size(1), longitude::binary-size(9), symbol_code::binary-size(1),
          comment::binary>> ->
          %{latitude: lat, longitude: lon} = parse_aprs_position(latitude, longitude)

          %{
            latitude: lat,
            longitude: lon,
            symbol_table_id: sym_table_id,
            symbol_code: symbol_code,
            comment: comment,
            position_format: :uncompressed
          }

        # Compressed position format
        <<"/", latitude_compressed::binary-size(4), longitude_compressed::binary-size(4), symbol_code::binary-size(1),
          cs::binary-size(2), compression_type::binary-size(1), comment::binary>> ->
          try do
            converted_lat = Parser.Helpers.convert_compressed_lat(latitude_compressed)
            converted_lon = Parser.Helpers.convert_compressed_lon(longitude_compressed)

            if is_number(converted_lat) and is_number(converted_lon) do
              compressed_cs = Parser.Helpers.convert_compressed_cs(cs)

              base_data = %{
                latitude: converted_lat,
                longitude: converted_lon,
                symbol_table_id: "/",
                symbol_code: symbol_code,
                comment: comment,
                position_format: :compressed,
                compression_type: compression_type
              }

              Map.merge(base_data, compressed_cs)
            else
              raise "Invalid compressed position"
            end
          rescue
            _ -> %{latitude: nil, longitude: nil, comment: comment, position_format: :compressed}
          end

        _ ->
          %{comment: rest, position_format: :unknown}
      end

    Map.merge(
      %{
        object_name: String.trim(object_name),
        live_killed: live_killed,
        timestamp: timestamp,
        data_type: :object
      },
      position_data
    )
  end

  @spec parse_object(String.t()) :: map()
  def parse_object(data) do
    %{
      raw_data: data,
      data_type: :object
    }
  end

  # Item Report parsing
  @spec parse_item(binary()) :: map()
  def parse_item(<<item_indicator, item_name_and_data::binary>>) when item_indicator in [?%, ?)] do
    # Items can have up to 9 character names, followed by ! for position or _ for killed
    case Regex.run(~r/^(.{1,9})([\!\_])(.*)$/, item_name_and_data) do
      [_, item_name, status_char, position_data] ->
        parsed_position = parse_item_position(position_data)

        base_data = %{
          item_name: String.trim(item_name),
          live_killed: status_char,
          data_type: :item
        }

        Map.merge(base_data, parsed_position)

      _ ->
        %{
          item_name: item_name_and_data,
          raw_data: <<item_indicator>> <> item_name_and_data,
          data_type: :item
        }
    end
  end

  def parse_item(data) do
    %{
      raw_data: data,
      data_type: :item
    }
  end

  # Parse item position data (similar to object position parsing)
  @spec parse_item_position(String.t()) :: map()
  defp parse_item_position(position_data) do
    case position_data do
      # Uncompressed position format
      <<latitude::binary-size(8), sym_table_id::binary-size(1), longitude::binary-size(9), symbol_code::binary-size(1),
        comment::binary>> ->
        %{latitude: lat, longitude: lon} = parse_aprs_position(latitude, longitude)

        %{
          latitude: lat,
          longitude: lon,
          symbol_table_id: sym_table_id,
          symbol_code: symbol_code,
          comment: comment,
          position_format: :uncompressed
        }

      # Compressed position format
      <<"/", latitude_compressed::binary-size(4), longitude_compressed::binary-size(4), symbol_code::binary-size(1),
        cs::binary-size(2), compression_type::binary-size(1), comment::binary>> ->
        converted_lat = Parser.Helpers.convert_compressed_lat(latitude_compressed)
        converted_lon = Parser.Helpers.convert_compressed_lon(longitude_compressed)
        compressed_cs = Parser.Helpers.convert_compressed_cs(cs)

        base_data = %{
          latitude: converted_lat,
          longitude: converted_lon,
          symbol_table_id: "/",
          symbol_code: symbol_code,
          comment: comment,
          position_format: :compressed,
          compression_type: compression_type
        }

        Map.merge(base_data, compressed_cs)

      _ ->
        %{
          comment: position_data,
          position_format: :unknown
        }
    end
  end

  # Weather Report parsing
  def parse_weather(<<"_", timestamp::binary-size(8), rest::binary>>) do
    weather_data = parse_weather_data(rest)

    Map.merge(
      %{
        timestamp: timestamp,
        data_type: :weather
      },
      weather_data
    )
  end

  @spec parse_weather(String.t()) :: map()
  def parse_weather(data) do
    weather_data = parse_weather_data(data)

    Map.merge(
      %{
        data_type: :weather
      },
      weather_data
    )
  end

  # Comprehensive weather data parsing
  defp parse_weather_data(weather_data) do
    # Extract timestamp if present
    timestamp = Parser.Helpers.extract_timestamp(weather_data)
    weather_data = Parser.Helpers.remove_timestamp(weather_data)

    # Parse each weather component
    weather_values = %{
      wind_direction: Parser.Helpers.parse_wind_direction(weather_data),
      wind_speed: Parser.Helpers.parse_wind_speed(weather_data),
      wind_gust: Parser.Helpers.parse_wind_gust(weather_data),
      temperature: Parser.Helpers.parse_temperature(weather_data),
      rain_1h: Parser.Helpers.parse_rainfall_1h(weather_data),
      rain_24h: Parser.Helpers.parse_rainfall_24h(weather_data),
      rain_since_midnight: Parser.Helpers.parse_rainfall_since_midnight(weather_data),
      humidity: Parser.Helpers.parse_humidity(weather_data),
      pressure: Parser.Helpers.parse_pressure(weather_data),
      luminosity: Parser.Helpers.parse_luminosity(weather_data),
      snow: Parser.Helpers.parse_snow(weather_data)
    }

    # Build base result map
    result = %{
      timestamp: timestamp,
      data_type: :weather,
      raw_weather_data: weather_data
    }

    # Add non-nil weather values to result
    Enum.reduce(weather_values, result, fn {key, value}, acc ->
      if is_nil(value), do: acc, else: Map.put(acc, key, value)
    end)
  end

  # Telemetry parsing
  def parse_telemetry(<<"T#", rest::binary>>) do
    case String.split(rest, ",") do
      [seq | [_ | _] = values] ->
        analog_values = Enum.take(values, 5)
        digital_values = values |> Enum.drop(5) |> Enum.take(8)

        %{
          sequence_number: Parser.Helpers.parse_telemetry_sequence(seq),
          analog_values: Parser.Helpers.parse_analog_values(analog_values),
          digital_values: Parser.Helpers.parse_digital_values(digital_values),
          data_type: :telemetry,
          raw_data: rest
        }

      _ ->
        %{
          raw_data: rest,
          data_type: :telemetry
        }
    end
  end

  # Handle telemetry parameter definitions (PARM)
  def parse_telemetry(<<":PARM.", rest::binary>>) do
    %{
      data_type: :telemetry_parameters,
      parameter_names: String.split(rest, ",", trim: true),
      raw_data: rest
    }
  end

  # Handle telemetry equation definitions (EQNS)
  def parse_telemetry(<<":EQNS.", rest::binary>>) do
    equations =
      rest
      |> String.split(",", trim: true)
      |> Enum.chunk_every(3)
      |> Enum.map(fn [a, b, c] ->
        %{
          a: Parser.Helpers.parse_coefficient(a),
          b: Parser.Helpers.parse_coefficient(b),
          c: Parser.Helpers.parse_coefficient(c)
        }
      end)

    %{
      data_type: :telemetry_equations,
      equations: equations,
      raw_data: rest
    }
  end

  # Handle telemetry unit definitions (UNIT)
  def parse_telemetry(<<":UNIT.", rest::binary>>) do
    %{
      data_type: :telemetry_units,
      units: String.split(rest, ",", trim: true),
      raw_data: rest
    }
  end

  # Handle telemetry bit sense definitions (BITS)
  def parse_telemetry(<<":BITS.", rest::binary>>) do
    case String.split(rest, ",", trim: true) do
      [bits_sense | project_names] ->
        %{
          data_type: :telemetry_bits,
          bits_sense: String.to_charlist(bits_sense),
          project_names: project_names,
          raw_data: rest
        }

      [] ->
        %{
          data_type: :telemetry_bits,
          bits_sense: [],
          project_names: [],
          raw_data: rest
        }
    end
  end

  @spec parse_telemetry(String.t()) :: map()
  def parse_telemetry(data) do
    %{
      raw_data: data,
      data_type: :telemetry
    }
  end

  # Station Capabilities parsing
  def parse_station_capabilities(<<"<", capabilities::binary>>) do
    %{
      capabilities: capabilities,
      data_type: :station_capabilities
    }
  end

  @spec parse_station_capabilities(String.t()) :: map()
  def parse_station_capabilities(data) do
    %{
      capabilities: data,
      data_type: :station_capabilities
    }
  end

  # Query parsing
  def parse_query(<<"?", query_type::binary-size(1), query_data::binary>>) do
    %{
      query_type: query_type,
      query_data: query_data,
      data_type: :query
    }
  end

  @spec parse_query(String.t()) :: map()
  def parse_query(data) do
    %{
      query_data: data,
      data_type: :query
    }
  end

  # User Defined parsing
  def parse_user_defined(<<"{", user_id::binary-size(1), user_data::binary>>) do
    parsed_data = parse_user_defined_format(user_id, user_data)

    Map.merge(
      %{
        user_id: user_id,
        data_type: :user_defined,
        raw_data: user_data
      },
      parsed_data
    )
  end

  @spec parse_user_defined(String.t()) :: map()
  def parse_user_defined(data) do
    %{
      user_data: data,
      data_type: :user_defined
    }
  end

  # Parse specific user-defined formats
  defp parse_user_defined_format("A", user_data), do: %{format: :experimental_a, content: user_data}

  defp parse_user_defined_format("B", user_data), do: %{format: :experimental_b, content: user_data}

  defp parse_user_defined_format("C", user_data), do: %{format: :custom_c, content: user_data}
  defp parse_user_defined_format(_, user_data), do: %{format: :unknown, content: user_data}

  # Third Party Traffic parsing
  def parse_third_party_traffic(packet) do
    if Parser.Helpers.count_leading_braces(packet) + 1 > 3 do
      %{
        error: "Maximum tunnel depth exceeded"
      }
    else
      case parse_tunneled_packet(packet) do
        {:ok, parsed_packet} ->
          build_third_party_traffic_result(packet, parsed_packet)

        {:error, reason} ->
          %{
            error: reason
          }
      end
    end
  end

  defp build_third_party_traffic_result(packet, parsed_packet) do
    case parse_nested_tunnel(packet) do
      {:ok, nested_packet} ->
        %{
          third_party_packet: nested_packet,
          data_type: :third_party_traffic,
          raw_data: packet
        }

      {:error, _} ->
        %{
          third_party_packet: parsed_packet,
          data_type: :third_party_traffic,
          raw_data: packet
        }
    end
  end

  @spec parse_tunneled_packet(String.t()) :: {:ok, map()} | {:error, String.t()}
  defp parse_tunneled_packet(packet) do
    case String.split(packet, ":", parts: 2) do
      [header, information] ->
        parse_tunneled_packet_with_header(header, information)

      _ ->
        {:error, "Invalid tunneled packet format"}
    end
  end

  defp parse_tunneled_packet_with_header(header, information) do
    case parse_tunneled_header(header) do
      {:ok, header_data} ->
        parse_tunneled_packet_with_information(header_data, information)

      {:error, reason} ->
        {:error, "Invalid header: #{reason}"}
    end
  end

  defp parse_tunneled_packet_with_information(header_data, information) do
    case parse_datatype_safe(information) do
      {:ok, data_type} ->
        data_without_type = String.slice(information, 1..-1//1)
        data_extended = parse_data(data_type, header_data.destination, data_without_type)

        {:ok,
         Map.merge(header_data, %{
           information_field: information,
           data_type: data_type,
           data_extended: data_extended
         })}

      {:error, reason} ->
        {:error, "Invalid data type: #{reason}"}
    end
  end

  @spec parse_tunneled_header(String.t()) :: {:ok, map()} | {:error, String.t()}
  defp parse_tunneled_header(header) do
    case String.split(header, ">", parts: 2) do
      [sender, path] ->
        case parse_callsign(sender) do
          {:ok, callsign_parts} ->
            base_callsign = List.first(callsign_parts)
            ssid = extract_ssid(List.last(callsign_parts))

            case split_path_for_tunnel(path) do
              {:ok, [destination, digi_path]} ->
                {:ok,
                 %{
                   sender: sender,
                   base_callsign: base_callsign,
                   ssid: ssid,
                   destination: destination,
                   digi_path: digi_path
                 }}

              {:error, reason} ->
                {:error, "Invalid path: #{reason}"}
            end

          {:error, reason} ->
            {:error, "Invalid callsign: #{reason}"}
        end

      _ ->
        {:error, "Invalid header format"}
    end
  end

  defp extract_ssid(nil), do: nil

  defp extract_ssid(s) when is_binary(s), do: s

  defp extract_ssid(i) when is_integer(i), do: to_string(i)
  defp extract_ssid(_), do: nil

  defp split_path_for_tunnel(path) do
    split_path(path)
  end

  # Add network tunneling support
  @spec parse_network_tunnel(String.t()) :: {:ok, map()} | {:error, String.t()}
  defp parse_network_tunnel(packet) do
    # Network tunneling packets start with "}" and contain a complete APRS packet
    case String.slice(packet, 1..-1//1) do
      tunneled_packet ->
        case parse_tunneled_packet(tunneled_packet) do
          {:ok, parsed_packet} ->
            {:ok,
             Map.merge(parsed_packet, %{
               tunnel_type: :network,
               raw_data: packet
             })}

          {:error, reason} ->
            {:error, "Invalid tunneled packet: #{reason}"}
        end
    end
  end

  # Add support for multiple levels of tunneling
  defp parse_nested_tunnel(packet, depth \\ 0) do
    cond do
      depth > 3 ->
        {:error, "Maximum tunnel depth exceeded"}

      String.starts_with?(packet, "}") ->
        case parse_network_tunnel(packet) do
          {:ok, parsed_packet} -> handle_parsed_network_tunnel(parsed_packet, depth)
          {:error, reason} -> {:error, reason}
        end

      true ->
        {:error, "Not a tunneled packet"}
    end
  end

  defp handle_parsed_network_tunnel(parsed_packet, depth) do
    case Map.get(parsed_packet, :data_extended) do
      %{raw_data: nested_data} when is_binary(nested_data) ->
        case parse_nested_tunnel(nested_data, depth + 1) do
          {:ok, nested_packet} -> {:ok, Map.put(parsed_packet, :nested_packet, nested_packet)}
          {:error, _} -> {:ok, parsed_packet}
        end

      _ ->
        {:ok, parsed_packet}
    end
  end

  # PHG Data parsing (Power, Height, Gain, Directivity)
  def parse_phg_data(<<"#", rest::binary>>) do
    parse_phg_data(rest)
  end

  def parse_phg_data(<<"PHG", p, h, g, d, rest::binary>>) do
    %{
      power: Parser.Helpers.parse_phg_power(p),
      height: Parser.Helpers.parse_phg_height(h),
      gain: Parser.Helpers.parse_phg_gain(g),
      directivity: Parser.Helpers.parse_phg_directivity(d),
      comment: rest,
      data_type: :phg_data
    }
  end

  def parse_phg_data(data) do
    %{raw_data: data, data_type: :phg_data}
  end

  # Add DAO (Datum) extension support
  @spec parse_dao_extension(String.t()) :: map() | nil
  defp parse_dao_extension(comment) do
    case Regex.run(~r/!([A-Za-z])([A-Za-z])([A-Za-z])!/, comment) do
      [_, lat_dao, lon_dao, _] ->
        %{
          lat_dao: lat_dao,
          lon_dao: lon_dao,
          datum: "WGS84"
        }

      _ ->
        nil
    end
  end
end

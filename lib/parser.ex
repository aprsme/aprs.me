defmodule Parser do
  @moduledoc """
  Main parsing library
  """
  # import Bitwise
  alias Aprs.Convert
  alias Parser.Types.MicE
  alias Parser.Types.Position

  require Logger

  def parse(message) do
    with {:ok, [sender, path, data]} <- split_packet(message),
         {:ok, [base_callsign, ssid]} <- parse_callsign(sender),
         {:ok, data_type} <- parse_datatype_safe(data),
         {:ok, [destination, path]} <- split_path(path) do
      data_trimmed = String.trim(data)
      data_extended = parse_data(data_type, destination, data_trimmed)

      {:ok,
       %{
         # TODO: temporary for liveview
         id: 16 |> :crypto.strong_rand_bytes() |> Base.encode16(case: :lower),
         sender: sender,
         path: path,
         destination: destination,
         information_field: data_trimmed,
         data_type: data_type,
         base_callsign: base_callsign,
         # Ensure ssid is never nil
         ssid: ssid || "0",
         data_extended: data_extended,
         # Set received_at when creating packet
         received_at: DateTime.truncate(DateTime.utc_now(), :microsecond)
       }}
    else
      {:error, reason} ->
        {:error, reason}

      _ ->
        {:error, "PARSE ERROR"}
    end
  rescue
    error ->
      Logger.debug("PARSE ERROR: #{inspect(error)} for message: #{message}")
      {:ok, file} = File.open("./badpackets.txt", [:append])
      IO.binwrite(file, message <> "\n\n")
      File.close(file)
      {:error, :invalid_packet}
  end

  # Safely split packet into components
  defp split_packet(message) do
    case String.split(message, [">", ":"], parts: 3) do
      [sender, path, data] when byte_size(sender) > 0 and byte_size(path) > 0 ->
        {:ok, [sender, path, data]}

      _ ->
        {:error, "Invalid packet format"}
    end
  end

  # Safely split path into destination and digipeater path
  defp split_path(path) do
    case String.split(path, ",", parts: 2) do
      [destination, digi_path] ->
        {:ok, [destination, digi_path]}

      [destination] ->
        {:ok, [destination, ""]}

      _ ->
        {:error, "Invalid path format"}
    end
  end

  # Safe version of parse_datatype that returns {:ok, type} or {:error, reason}
  defp parse_datatype_safe(data) do
    case String.first(data) do
      nil -> {:error, "Empty data field"}
      first_char -> {:ok, parse_datatype(first_char)}
    end
  end

  def parse_callsign(callsign) do
    cond do
      not is_binary(callsign) ->
        {:error, "Invalid callsign format"}

      byte_size(callsign) == 0 ->
        {:error, "Empty callsign"}

      byte_size(callsign) > 15 ->
        {:error, "Callsign too long"}

      String.contains?(callsign, "-") ->
        case String.split(callsign, "-") do
          [base, ssid] when byte_size(base) > 0 and byte_size(base) <= 6 ->
            case validate_ssid(ssid) do
              {:ok, valid_ssid} -> {:ok, [base, valid_ssid]}
              {:error, _} -> {:error, "Invalid SSID"}
            end

          _ ->
            {:error, "Invalid callsign-SSID format"}
        end

      byte_size(callsign) <= 6 ->
        {:ok, [callsign, "0"]}

      true ->
        {:error, "Invalid callsign"}
    end
  end

  # Validate SSID (0-15)
  defp validate_ssid(ssid) do
    case Integer.parse(ssid) do
      {num, ""} when num >= 0 and num <= 15 ->
        {:ok, Integer.to_string(num)}

      _ ->
        {:error, "SSID must be 0-15"}
    end
  end

  # One of the nutty exceptions in the APRS protocol has to do with this
  # data type indicator. It's usually the first character of the message.
  # However, in some rare cases, the ! indicator can be anywhere in the
  # first 40 characters of the message. I'm not going to deal with that
  # weird case right now. It seems like its for a specific type of old
  # TNC hardware that probably doesn't even exist anymore.
  def parse_datatype(datatype) when datatype == ":", do: :message
  def parse_datatype(datatype) when datatype == ">", do: :status
  def parse_datatype(datatype) when datatype == "!", do: :position
  def parse_datatype(datatype) when datatype == "/", do: :timestamped_position
  def parse_datatype(datatype) when datatype == "=", do: :position_with_message
  def parse_datatype(datatype) when datatype == "@", do: :timestamped_position_with_message
  def parse_datatype(datatype) when datatype == ";", do: :object
  def parse_datatype(datatype) when datatype == "`", do: :mic_e
  def parse_datatype(datatype) when datatype == "'", do: :mic_e_old
  def parse_datatype(datatype) when datatype == "_", do: :weather
  def parse_datatype(datatype) when datatype == "T", do: :telemetry
  def parse_datatype(datatype) when datatype == "$", do: :raw_gps_ultimeter
  def parse_datatype(datatype) when datatype == "<", do: :station_capabilities
  def parse_datatype(datatype) when datatype == "?", do: :query
  def parse_datatype(datatype) when datatype == "{", do: :user_defined
  def parse_datatype(datatype) when datatype == "}", do: :third_party_traffic
  def parse_datatype(datatype) when datatype == "%", do: :item
  def parse_datatype(datatype) when datatype == ")", do: :item
  def parse_datatype(datatype) when datatype == "*", do: :peet_logging
  def parse_datatype(datatype) when datatype == ",", do: :invalid_test_data
  def parse_datatype(datatype) when datatype == "#", do: :phg_data
  def parse_datatype(datatype) when datatype == "(", do: :unused
  def parse_datatype(datatype) when datatype == "&", do: :reserved

  def parse_datatype(_datatype), do: :unknown_datatype

  def parse_data(:mic_e, destination, data), do: parse_mic_e(destination, data)
  def parse_data(:mic_e_old, destination, data), do: parse_mic_e(destination, data)
  def parse_data(:position, _destination, data), do: parse_position_without_timestamp(false, data)

  def parse_data(:position_with_message, _destination, data), do: parse_position_without_timestamp(true, data)

  def parse_data(:timestamped_position, _destination, data), do: parse_position_with_timestamp(false, data)

  def parse_data(
        :timestamped_position_with_message,
        _destination,
        <<_dti::binary-size(1), date_time_position::binary-size(25), "_", weather_report::binary>>
      ) do
    parse_position_with_datetime_and_weather(true, date_time_position, weather_report)
  end

  def parse_data(:timestamped_position_with_message, _destination, data), do: parse_position_with_timestamp(true, data)

  def parse_data(:message, _destination, <<":", addressee::binary-size(9), ":", message_text::binary>>) do
    # Aprs messages can have an optional message number tacked onto the end
    # for the purposes of acknowledging message receipt.
    # The sender tacks the message number onto the end of the message,
    # and the receiving station is supposed to respond back with an
    # acknowledgement of that message number.
    # Example
    # Sender: Hello world{123
    # Receiver: ack123
    # Special thanks to Jeff Smith(https://github.com/electricshaman) for the regex
    regex = ~r/^(?<message>.*?)(?:{(?<message_number>\w+))?$/i
    result = find_matches(regex, message_text)

    message_text =
      case result["message"] do
        nil -> ""
        m -> String.trim(m)
      end

    %{
      to: String.trim(addressee),
      message_text: message_text,
      message_number: result["message_number"]
    }
  end

  def parse_data(:status, _destination, data), do: parse_status(data)
  def parse_data(:object, _destination, data), do: parse_object(data)
  def parse_data(:item, _destination, data), do: parse_item(data)
  def parse_data(:weather, _destination, data), do: parse_weather(data)
  def parse_data(:telemetry, _destination, data), do: parse_telemetry(data)
  def parse_data(:station_capabilities, _destination, data), do: parse_station_capabilities(data)
  def parse_data(:query, _destination, data), do: parse_query(data)
  def parse_data(:user_defined, _destination, data), do: parse_user_defined(data)
  def parse_data(:third_party_traffic, _destination, data), do: parse_third_party_traffic(data)
  def parse_data(:phg_data, _destination, data), do: parse_phg_data(data)
  def parse_data(:peet_logging, _destination, data), do: parse_peet_logging(data)
  def parse_data(:invalid_test_data, _destination, data), do: parse_invalid_test_data(data)

  def parse_data(_type, _destination, _data), do: nil

  def parse_position_with_datetime_and_weather(aprs_messaging?, date_time_position_data, weather_report) do
    <<time::binary-size(7), latitude::binary-size(8), sym_table_id::binary-size(1), longitude::binary-size(9)>> =
      date_time_position_data

    # position = Parser.Types.Position.from_aprs(latitude, longitude)
    %{latitude: lat, longitude: lon} = Position.from_aprs(latitude, longitude)

    %{
      latitude: lat,
      longitude: lon,
      timestamp: time,
      symbol_table_id: sym_table_id,
      symbol_code: "_",
      weather: weather_report,
      data_type: :position_with_datetime_and_weather,
      aprs_messaging?: aprs_messaging?
    }
  end

  def decode_compressed_position(
        <<"/", latitude::binary-size(4), longitude::binary-size(4), _symbol::binary-size(1), _cs::binary-size(2),
          _compression_type::binary-size(2), _rest::binary>>
      ) do
    lat = convert_to_base91(latitude)
    lon = convert_to_base91(longitude)
    [:ok, lat, lon]
  end

  def convert_to_base91(<<value::binary-size(4)>>) do
    [v1, v2, v3, v4] = to_charlist(value)
    (v1 - 33) * 91 * 91 * 91 + (v2 - 33) * 91 * 91 + (v3 - 33) * 91 + v4
  end

  def parse_position_without_timestamp(_aprs_messaging?, <<"!!", rest::binary>> = message) do
    # this is an ultimeter weather station. need to parse its weird format
    # {:ok, file} = File.open("badpackets.txt")
    # IO.puts(file, message)
    # File.close(file)

    # a = "0000000001FF000427C70002CCD30001026E003A050F00040000"

    [
      wind_speed,
      wind_direction,
      temp,
      rain_long_term_total,
      barrometer,
      barrometer_delta_value,
      barrometer_corr_factor_lsw,
      barrometer_corr_factor_msw,
      humidity,
      _day_of_year,
      _minute_of_day,
      today_rain_total,
      wind_speed_avg
    ] =
      rest
      |> String.codepoints()
      |> Enum.chunk_every(4)
      |> Enum.map(&Enum.join/1)
      |> Enum.map(&hex_decode(&1))

    %{
      wind_speed: Convert.wind(wind_speed, :ultimeter, :mph),
      wind_direction: wind_direction,
      temp_f: Convert.temp(temp, :ultimeter, :f),
      rain_long_term_total: rain_long_term_total,
      barrometer: barrometer,
      barrometer_delta_value: barrometer_delta_value,
      barrometer_corr_factor_lsw: barrometer_corr_factor_lsw,
      barrometer_corr_factor_msw: barrometer_corr_factor_msw,
      humidity: humidity,
      today_rain_total: today_rain_total,
      wind_speed_avg: wind_speed_avg
    }

    Logger.debug("TODO: PARSE ULTIMETER DATA: " <> message)
  end

  def parse_position_without_timestamp(
        aprs_messaging?,
        <<_dti::binary-size(1), latitude::binary-size(8), sym_table_id::binary-size(1), longitude::binary-size(9),
          symbol_code::binary-size(1), comment::binary>>
      ) do
    case validate_position_data(latitude, longitude) do
      {:ok, {lat, lon}} ->
        %{
          latitude: lat,
          longitude: lon,
          symbol_table_id: sym_table_id,
          symbol_code: symbol_code,
          comment: comment,
          data_type: :position,
          aprs_messaging?: aprs_messaging?
        }

      {:error, reason} ->
        Logger.warning("Invalid position data: #{reason}")

        %{
          latitude: nil,
          longitude: nil,
          symbol_table_id: sym_table_id,
          symbol_code: symbol_code,
          comment: comment,
          data_type: :invalid_position,
          aprs_messaging?: aprs_messaging?,
          error: reason
        }
    end
  rescue
    e ->
      Logger.error(Exception.format(:error, e, __STACKTRACE__))

      %{
        latitude: nil,
        longitude: nil,
        data_type: :parse_error,
        error: "Position parsing failed"
      }
  end

  def parse_position_without_timestamp(
        aprs_messaging?,
        <<_dti::binary-size(1), "/", latitude::binary-size(4), longitude::binary-size(4), symbol_code::binary-size(1),
          cs::binary-size(2), _compression_type::binary-size(1), comment::binary>> = _message
      ) do
    # Parse compressed latitude and longitude
    converted_lat = convert_compressed_lat(latitude)
    converted_lon = convert_compressed_lon(longitude)
    position = Position.from_decimal(converted_lat, converted_lon)

    # Parse compressed course/speed or range data
    compressed_cs = convert_compressed_cs(cs)

    # In compressed format, the symbol is a single character that represents both
    # the symbol table and symbol code. For proper APRS compatibility, we would
    # need to decode this, but for now we'll treat it as the symbol code.
    result = %{
      latitude: converted_lat,
      longitude: converted_lon,
      position: position,
      # Compressed format uses "/" as default table
      symbol_table_id: "/",
      symbol_code: symbol_code,
      comment: comment,
      data_type: :position,
      aprs_messaging?: aprs_messaging?
    }

    # Add course/speed or range information if available
    Map.merge(result, compressed_cs)
  end

  # Catch-all pattern for malformed position packets
  def parse_position_without_timestamp(aprs_messaging?, <<_dti::binary-size(1), rest::binary>> = data) do
    Logger.warning("Malformed position packet: #{inspect(data)}")

    %{
      latitude: nil,
      longitude: nil,
      symbol_table_id: nil,
      symbol_code: nil,
      comment: rest,
      data_type: :malformed_position,
      aprs_messaging?: aprs_messaging?,
      raw_data: data
    }
  end

  def parse_position_with_timestamp(
        aprs_messaging?,
        <<_dti::binary-size(1), time::binary-size(7), latitude::binary-size(8), sym_table_id::binary-size(1),
          longitude::binary-size(9), symbol_code::binary-size(1), comment::binary>>
      ) do
    case validate_position_data(latitude, longitude) do
      {:ok, {lat, lon}} ->
        position = Position.from_aprs(latitude, longitude)

        %{
          latitude: lat,
          longitude: lon,
          position: position,
          time: validate_timestamp(time),
          symbol_table_id: sym_table_id,
          symbol_code: symbol_code,
          comment: comment,
          data_type: :position,
          aprs_messaging?: aprs_messaging?
        }

      {:error, reason} ->
        Logger.warning("Invalid timestamped position data: #{reason}")

        %{
          latitude: nil,
          longitude: nil,
          time: time,
          symbol_table_id: sym_table_id,
          symbol_code: symbol_code,
          comment: comment,
          data_type: :invalid_position,
          aprs_messaging?: aprs_messaging?,
          error: reason
        }
    end
  rescue
    e ->
      Logger.error(Exception.format(:error, e, __STACKTRACE__))

      %{
        latitude: nil,
        longitude: nil,
        data_type: :parse_error,
        error: "Timestamped position parsing failed"
      }
  end

  def parse_mic_e(destination_field, information_field) do
    # Logger.debug("MIC-E: " <> destination_field <> " :: " <> information_field)
    # Mic-E is kind of a nutty compression scheme, APRS packs additional
    # information into the destination field when Mic-E encoding is used.
    # No other aprs packets use the destination field this way as far as i know.

    # The destination field contains the following information:
    # Latitude, message code, N/S & E/W indicators, longitude offset, digipath code
    destination_data = parse_mic_e_destination(destination_field)

    information_data =
      parse_mic_e_information(information_field, destination_data.longitude_offset)

    %MicE{
      lat_degrees: destination_data.lat_degrees,
      lat_minutes: destination_data.lat_minutes,
      lat_fractional: destination_data.lat_fractional,
      lat_direction: destination_data.lat_direction,
      lon_direction: destination_data.lon_direction,
      longitude_offset: destination_data.longitude_offset,
      message_code: destination_data.message_code,
      message_description: destination_data.message_description,
      dti: information_data.dti,
      heading: information_data.heading,
      lon_degrees: information_data.lon_degrees,
      lon_minutes: information_data.lon_minutes,
      lon_fractional: information_data.lon_fractional,
      speed: information_data.speed,
      manufacturer: information_data.manufacturer,
      message: information_data.message
    }
  end

  def parse_mic_e_digit(<<c>>) when c in ?0..?9, do: [c - ?0, 0, nil]
  def parse_mic_e_digit(<<c>>) when c in ?A..?J, do: [c - ?A, 1, :custom]
  def parse_mic_e_digit(<<c>>) when c in ?P..?Y, do: [c - ?P, 1, :standard]

  def parse_mic_e_digit("K"), do: [0, 1, :custom]
  def parse_mic_e_digit("L"), do: [0, 0, nil]
  def parse_mic_e_digit("Z"), do: [0, 1, :standard]

  def parse_mic_e_digit(_c), do: [:unknown, :unknown, :unknown]

  def parse_mic_e_destination(destination_field) do
    digits =
      destination_field
      |> String.codepoints()
      |> Enum.map(&parse_mic_e_digit/1)
      |> Enum.map(&hd/1)

    deg = digits |> Enum.slice(0..1) |> Enum.join() |> String.to_integer()
    min = digits |> Enum.slice(2..3) |> Enum.join() |> String.to_integer()
    fractional = digits |> Enum.slice(4..5) |> Enum.join() |> String.to_integer()

    [ns, lo, ew] = destination_field |> to_charlist() |> Enum.slice(3..5)

    north_south_indicator =
      case ns do
        x when x in ?0..?9 -> :south
        x when x == ?L -> :south
        x when x in ?P..?Z -> :north
        _ -> :unknown
      end

    east_west_indicator =
      case ew do
        x when x in ?0..?9 -> :east
        x when x == ?L -> :east
        x when x in ?P..?Z -> :west
        _ -> :unknown
      end

    longitude_offset =
      case lo do
        x when x in ?0..?9 -> 0
        x when x == ?L -> 0
        x when x in ?P..?Z -> 100
        _ -> :unknown
      end

    statuses = [
      "Emergency",
      "Priority",
      "Special",
      "Committed",
      "Returning",
      "In Service",
      "En Route",
      "Off Duty"
    ]

    message_digits =
      destination_field
      |> String.codepoints()
      |> Enum.take(3)

    [_, message_bit_1, message_type] = parse_mic_e_digit(Enum.at(message_digits, 0))
    [_, message_bit_2, _] = parse_mic_e_digit(Enum.at(message_digits, 1))
    [_, message_bit_3, _] = parse_mic_e_digit(Enum.at(message_digits, 2))

    # Convert the bits to binary to get the array index
    index = message_bit_1 * 4 + message_bit_2 * 2 + message_bit_3
    # need to invert this from the actual array index
    display_index = (7 - index) |> to_string() |> String.pad_leading(2, "0")

    [message_code, message_description] =
      case message_type do
        :standard ->
          ["M" <> display_index, Enum.at(statuses, index)]

        :custom ->
          ["C" <> display_index, "Custom-#{display_index}"]

        nil ->
          ["", Enum.at(statuses, index)]
      end

    %{
      lat_degrees: deg,
      lat_minutes: min,
      lat_fractional: fractional,
      lat_direction: north_south_indicator,
      lon_direction: east_west_indicator,
      longitude_offset: longitude_offset,
      message_code: message_code,
      message_description: message_description
    }
  end

  def parse_mic_e_information(
        <<dti::binary-size(1), d28::integer, m28::integer, f28::integer, sp28::integer, dc28::integer, se28::integer,
          symbol::binary-size(1), table::binary-size(1), message::binary>> = _information_field,
        longitude_offset
      ) do
    m =
      case m28 - 28 do
        x when x >= 60 -> x - 60
        x -> x
      end

    sp =
      case sp28 - 28 do
        x when x >= 80 -> x - 80
        x -> x
      end

    dc = dc28 - 28
    quotient = div(dc, 10)
    remainder = rem(dc, 10)
    dc = sp * 10 + quotient
    heading = (remainder - 4) * 100 + (se28 - 28)

    # Messages should at least have a starting and ending symbol, and an optional message in between
    # But, there might not be any symbols either, so it could look like any of the following:
    # >^  <- TH-D74
    # nil <- who knows
    # ]\"55}146.820 MHz T103 -0600= <- Kenwood DM-710

    regex = ~r/^(?<first>.?)(?<msg>.*)(?<secondtolast>.)(?<last>.)$/i
    result = find_matches(regex, message)

    symbol1 =
      if result["first"] == "" do
        result["secondtolast"]
      else
        result["first"]
      end

    manufacturer = parse_manufacturer(symbol1, result["secondtolast"], result["last"])

    %{
      dti: dti,
      lon_degrees: d28 - 28 + longitude_offset,
      lon_minutes: m,
      lon_fractional: f28 - 28,
      speed: dc,
      heading: heading,
      symbol: symbol,
      table: table,
      manufacturer: manufacturer,
      message: message
    }
  end

  def parse_manufacturer(" ", _s2, _s3), do: "Original MIC-E"
  def parse_manufacturer(">", _s2, "="), do: "Kenwood TH-D72"
  def parse_manufacturer(">", _s2, "^"), do: "Kenwood TH-D74"
  def parse_manufacturer(">", _s2, _s3), do: "Kenwood TH-D74A"
  def parse_manufacturer("]", _s2, "="), do: "Kenwood DM-710"
  def parse_manufacturer("]", _s2, _s3), do: "Kenwood DM-700"
  def parse_manufacturer("`", "_", " "), do: "Yaesu VX-8"
  def parse_manufacturer("`", "_", "\""), do: "Yaesu FTM-350"
  def parse_manufacturer("`", "_", "#"), do: "Yaesu VX-8G"
  def parse_manufacturer("`", "_", "$"), do: "Yaesu FT1D"
  def parse_manufacturer("`", "_", "%"), do: "Yaesu FTM-400DR"
  def parse_manufacturer("`", "_", ")"), do: "Yaesu FTM-100D"
  def parse_manufacturer("`", "_", "("), do: "Yaesu FT2D"
  def parse_manufacturer("`", " ", "X"), do: "AP510"
  def parse_manufacturer("`", _s2, _s3), do: "Mic-Emsg"
  def parse_manufacturer("'", "|", "3"), do: "Byonics TinyTrack3"
  def parse_manufacturer("'", "|", "4"), do: "Byonics TinyTrack4"
  def parse_manufacturer("'", ":", "4"), do: "SCS GmbH & Co. P4dragon DR-7400 modems"
  def parse_manufacturer("'", ":", "8"), do: "SCS GmbH & Co. P4dragon DR-7800 modems"
  def parse_manufacturer("'", _s2, _s3), do: "McTrackr"
  def parse_manufacturer(_s1, "\"", _s3), do: "Hamhud ?"
  def parse_manufacturer(_s1, "/", _s3), do: "Argent ?"
  def parse_manufacturer(_s1, "^", _s3), do: "HinzTec anyfrog"
  def parse_manufacturer(_s1, "*", _s3), do: "APOZxx www.KissOZ.dk Tracker. OZ1EKD and OZ7HVO"
  def parse_manufacturer(_s1, "~", _s3), do: "Other"
  def parse_manufacturer(_symbol1, _symbol2, _symbol3), do: :unknown_manufacturer

  defp find_matches(regex, text) do
    case Regex.names(regex) do
      [] ->
        matches = Regex.run(regex, text)

        Enum.reduce(Enum.with_index(matches), %{}, fn {match, index}, acc ->
          Map.put(acc, index, match)
        end)

      _ ->
        Regex.named_captures(regex, text)
    end
  end

  defp hex_decode(input) do
    {result, ""} = Integer.parse(input, 16)
    result
  end

  # defp convert_ultimeter_humidity(hum) do
  #   hum * 10
  # end

  def convert_compressed_lat(lat) do
    [l1, l2, l3, l4] = to_charlist(lat)
    90 - ((l1 - 33) * 91 ** 3 + (l2 - 33) * 91 ** 2 + (l3 - 33) * 91 + l4 - 33) / 380_926
  end

  def convert_compressed_lon(lon) do
    [l1, l2, l3, l4] = to_charlist(lon)
    -180 + ((l1 - 33) * 91 ** 3 + (l2 - 33) * 91 ** 2 + (l3 - 33) * 91 + l4 - 33) / 190_463
  end

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
        IO.inspect(cs, label: "not parsable")

        %{
          course: 0,
          speed: 0
        }
    end
  end

  # Status Report parsing
  def parse_status(<<">", status_text::binary>>) do
    %{
      status_text: status_text,
      data_type: :status
    }
  end

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
          %{latitude: lat, longitude: lon} = Position.from_aprs(latitude, longitude)

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
          converted_lat = convert_compressed_lat(latitude_compressed)
          converted_lon = convert_compressed_lon(longitude_compressed)
          compressed_cs = convert_compressed_cs(cs)

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

  def parse_object(data) do
    %{
      raw_data: data,
      data_type: :object
    }
  end

  # Item Report parsing
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
  defp parse_item_position(position_data) do
    case position_data do
      # Uncompressed position format
      <<latitude::binary-size(8), sym_table_id::binary-size(1), longitude::binary-size(9), symbol_code::binary-size(1),
        comment::binary>> ->
        %{latitude: lat, longitude: lon} = Position.from_aprs(latitude, longitude)

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
        converted_lat = convert_compressed_lat(latitude_compressed)
        converted_lon = convert_compressed_lon(longitude_compressed)
        compressed_cs = convert_compressed_cs(cs)

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
  defp parse_weather_data(weather_string) do
    %{}
    |> parse_wind_data(weather_string)
    |> parse_temperature_data(weather_string)
    |> parse_rain_data(weather_string)
    |> parse_humidity_data(weather_string)
    |> parse_barometric_data(weather_string)
    |> parse_snow_data(weather_string)
    |> parse_other_weather_data(weather_string)
    |> Map.put(:raw_weather_data, weather_string)
  end

  # Parse wind direction and speed (ddd/sss format)
  defp parse_wind_data(data, weather_string) do
    case Regex.run(~r/(\d{3})\/(\d{3})/, weather_string) do
      [_, direction, speed] ->
        data
        |> Map.put(:wind_direction, String.to_integer(direction))
        |> Map.put(:wind_speed, String.to_integer(speed))

      _ ->
        data
    end
  end

  # Parse temperature (tTTT format, in Fahrenheit)
  defp parse_temperature_data(data, weather_string) do
    case Regex.run(~r/t(-?\d{3})/, weather_string) do
      [_, temp] ->
        temp_f = String.to_integer(temp)
        temp_c = (temp_f - 32) * 5 / 9

        data
        |> Map.put(:temperature_f, temp_f)
        |> Map.put(:temperature_c, Float.round(temp_c, 1))

      _ ->
        data
    end
  end

  # Parse rainfall data
  defp parse_rain_data(data, weather_string) do
    data
    |> parse_rain_field(weather_string, ~r/r(\d{3})/, :rain_1h)
    |> parse_rain_field(weather_string, ~r/p(\d{3})/, :rain_24h)
    |> parse_rain_field(weather_string, ~r/P(\d{3})/, :rain_today)
  end

  defp parse_rain_field(data, weather_string, regex, field) do
    case Regex.run(regex, weather_string) do
      [_, rain] ->
        rain_hundredths = String.to_integer(rain)
        rain_inches = rain_hundredths / 100.0
        Map.put(data, field, rain_inches)

      _ ->
        data
    end
  end

  # Parse humidity (hHH format)
  defp parse_humidity_data(data, weather_string) do
    case Regex.run(~r/h(\d{2})/, weather_string) do
      [_, humidity] ->
        humidity_val = String.to_integer(humidity)
        # Handle special case where 00 means 100%
        humidity_percent = if humidity_val == 0, do: 100, else: humidity_val
        Map.put(data, :humidity, humidity_percent)

      _ ->
        data
    end
  end

  # Parse barometric pressure (bBBBBB format, in tenths of millibars)
  defp parse_barometric_data(data, weather_string) do
    case Regex.run(~r/b(\d{5})/, weather_string) do
      [_, pressure] ->
        pressure_tenths_mb = String.to_integer(pressure)
        pressure_mb = pressure_tenths_mb / 10.0
        pressure_inhg = pressure_mb * 0.02953

        data
        |> Map.put(:barometric_pressure_mb, pressure_mb)
        |> Map.put(:barometric_pressure_inhg, Float.round(pressure_inhg, 2))

      _ ->
        data
    end
  end

  # Parse snow data (sSS format, in inches)
  defp parse_snow_data(data, weather_string) do
    case Regex.run(~r/s(\d{3})/, weather_string) do
      [_, snow] ->
        snow_inches = String.to_integer(snow)
        Map.put(data, :snow_24h, snow_inches)

      _ ->
        data
    end
  end

  # Parse other weather data (luminosity, etc.)
  defp parse_other_weather_data(data, weather_string) do
    data
    |> parse_luminosity(weather_string)
    |> parse_wind_gust(weather_string)
  end

  # Parse luminosity (LLLLL format)
  defp parse_luminosity(data, weather_string) do
    case Regex.run(~r/L(\d{3})/, weather_string) do
      [_, luminosity] ->
        Map.put(data, :luminosity, String.to_integer(luminosity))

      _ ->
        data
    end
  end

  # Parse wind gust (gGGG format)
  defp parse_wind_gust(data, weather_string) do
    case Regex.run(~r/g(\d{3})/, weather_string) do
      [_, gust] ->
        Map.put(data, :wind_gust, String.to_integer(gust))

      _ ->
        data
    end
  end

  # Telemetry parsing
  def parse_telemetry(<<"T", rest::binary>>) do
    case String.split(rest, ",") do
      [seq | [_ | _] = values] ->
        analog_values = Enum.take(values, 5)
        digital_values = values |> Enum.drop(5) |> Enum.take(8)

        %{
          sequence_number: parse_telemetry_sequence(seq),
          analog_values: parse_analog_values(analog_values),
          digital_values: parse_digital_values(digital_values),
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
    parse_telemetry_parameters(rest)
  end

  # Handle telemetry equation definitions (EQNS)
  def parse_telemetry(<<":EQNS.", rest::binary>>) do
    parse_telemetry_equations(rest)
  end

  # Handle telemetry unit definitions (UNIT)
  def parse_telemetry(<<":UNIT.", rest::binary>>) do
    parse_telemetry_units(rest)
  end

  # Handle telemetry bit sense definitions (BITS)
  def parse_telemetry(<<":BITS.", rest::binary>>) do
    parse_telemetry_bits(rest)
  end

  def parse_telemetry(data) do
    %{
      raw_data: data,
      data_type: :telemetry
    }
  end

  # Parse telemetry sequence number
  defp parse_telemetry_sequence(seq) do
    case Integer.parse(seq) do
      {num, _} -> num
      :error -> seq
    end
  end

  # Parse analog values (convert to floats where possible)
  defp parse_analog_values(values) do
    Enum.map(values, fn value ->
      case Float.parse(value) do
        {float_val, _} ->
          float_val

        :error ->
          case Integer.parse(value) do
            {int_val, _} -> int_val
            :error -> value
          end
      end
    end)
  end

  # Parse digital values (convert to integers where possible)
  defp parse_digital_values(values) do
    Enum.map(values, fn value ->
      case Integer.parse(value) do
        {int_val, _} -> int_val
        :error -> value
      end
    end)
  end

  # Parse telemetry parameter names
  defp parse_telemetry_parameters(params_string) do
    parameters = String.split(params_string, ",", trim: true)

    %{
      parameter_names: parameters,
      data_type: :telemetry_parameters
    }
  end

  # Parse telemetry equations (a,b,c coefficients for each channel)
  defp parse_telemetry_equations(eqns_string) do
    equations =
      eqns_string
      |> String.split(",", trim: true)
      |> Enum.chunk_every(3)
      |> Enum.map(fn [a, b, c] ->
        %{
          a: parse_coefficient(a),
          b: parse_coefficient(b),
          c: parse_coefficient(c)
        }
      end)

    %{
      equations: equations,
      data_type: :telemetry_equations
    }
  end

  # Parse telemetry units
  defp parse_telemetry_units(units_string) do
    units = String.split(units_string, ",", trim: true)

    %{
      units: units,
      data_type: :telemetry_units
    }
  end

  # Parse telemetry bit sense and project names
  defp parse_telemetry_bits(bits_string) do
    [bits_sense | project_names] = String.split(bits_string, ",", trim: true)

    %{
      bits_sense: bits_sense,
      project_names: project_names,
      data_type: :telemetry_bits
    }
  end

  # Parse equation coefficient
  defp parse_coefficient(coeff) do
    case Float.parse(coeff) do
      {float_val, _} ->
        float_val

      :error ->
        case Integer.parse(coeff) do
          {int_val, _} -> int_val
          :error -> coeff
        end
    end
  end

  # Station Capabilities parsing
  def parse_station_capabilities(<<"<", capabilities::binary>>) do
    %{
      capabilities: capabilities,
      data_type: :station_capabilities
    }
  end

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

  def parse_user_defined(data) do
    %{
      user_data: data,
      data_type: :user_defined
    }
  end

  # Parse specific user-defined formats
  defp parse_user_defined_format(user_id, user_data) do
    case user_id do
      "A" ->
        # Experimental format A
        %{format: :experimental_a, content: user_data}

      "B" ->
        # Experimental format B
        %{format: :experimental_b, content: user_data}

      "C" ->
        # Custom format C
        %{format: :custom_c, content: user_data}

      _ ->
        # Unknown user-defined format
        %{format: :unknown, content: user_data}
    end
  end

  # Third Party Traffic parsing
  def parse_third_party_traffic(<<"}", third_party_packet::binary>>) do
    # Third party traffic contains a complete APRS packet
    case String.split(third_party_packet, ":", parts: 2) do
      [header, information] ->
        case String.split(header, ">", parts: 2) do
          [sender, path] ->
            %{
              third_party_sender: sender,
              third_party_path: path,
              third_party_information: information,
              data_type: :third_party_traffic,
              raw_data: third_party_packet
            }

          _ ->
            %{
              third_party_data: third_party_packet,
              data_type: :third_party_traffic
            }
        end

      _ ->
        %{
          third_party_data: third_party_packet,
          data_type: :third_party_traffic
        }
    end
  end

  def parse_third_party_traffic(data) do
    %{
      third_party_data: data,
      data_type: :third_party_traffic
    }
  end

  # PHG Data parsing (Power, Height, Gain, Directivity)
  def parse_phg_data(<<"#", phg_data::binary>>) do
    case phg_data do
      <<"PHG", power::binary-size(1), height::binary-size(1), gain::binary-size(1), directivity::binary-size(1),
        rest::binary>> ->
        %{
          power: parse_phg_power(power),
          height: parse_phg_height(height),
          gain: parse_phg_gain(gain),
          directivity: parse_phg_directivity(directivity),
          comment: rest,
          data_type: :phg_data
        }

      <<"DFS", strength::binary-size(1), height::binary-size(1), gain::binary-size(1), directivity::binary-size(1),
        rest::binary>> ->
        %{
          df_strength: parse_df_strength(strength),
          height: parse_phg_height(height),
          gain: parse_phg_gain(gain),
          directivity: parse_phg_directivity(directivity),
          comment: rest,
          data_type: :df_report
        }

      _ ->
        %{
          phg_data: phg_data,
          data_type: :phg_data
        }
    end
  end

  def parse_phg_data(data) do
    %{
      phg_data: data,
      data_type: :phg_data
    }
  end

  # PHG Power conversion (0-9)
  defp parse_phg_power("0"), do: {0, "0 watts"}
  defp parse_phg_power("1"), do: {1, "1 watt"}
  defp parse_phg_power("2"), do: {4, "4 watts"}
  defp parse_phg_power("3"), do: {9, "9 watts"}
  defp parse_phg_power("4"), do: {16, "16 watts"}
  defp parse_phg_power("5"), do: {25, "25 watts"}
  defp parse_phg_power("6"), do: {36, "36 watts"}
  defp parse_phg_power("7"), do: {49, "49 watts"}
  defp parse_phg_power("8"), do: {64, "64 watts"}
  defp parse_phg_power("9"), do: {81, "81 watts"}
  defp parse_phg_power(p), do: {nil, "Unknown power: #{p}"}

  # PHG Height conversion (0-9)
  defp parse_phg_height("0"), do: {10, "10 feet"}
  defp parse_phg_height("1"), do: {20, "20 feet"}
  defp parse_phg_height("2"), do: {40, "40 feet"}
  defp parse_phg_height("3"), do: {80, "80 feet"}
  defp parse_phg_height("4"), do: {160, "160 feet"}
  defp parse_phg_height("5"), do: {320, "320 feet"}
  defp parse_phg_height("6"), do: {640, "640 feet"}
  defp parse_phg_height("7"), do: {1280, "1280 feet"}
  defp parse_phg_height("8"), do: {2560, "2560 feet"}
  defp parse_phg_height("9"), do: {5120, "5120 feet"}
  defp parse_phg_height(h), do: {nil, "Unknown height: #{h}"}

  # PHG Gain conversion (0-9)
  defp parse_phg_gain("0"), do: {0, "0 dB"}
  defp parse_phg_gain("1"), do: {1, "1 dB"}
  defp parse_phg_gain("2"), do: {2, "2 dB"}
  defp parse_phg_gain("3"), do: {3, "3 dB"}
  defp parse_phg_gain("4"), do: {4, "4 dB"}
  defp parse_phg_gain("5"), do: {5, "5 dB"}
  defp parse_phg_gain("6"), do: {6, "6 dB"}
  defp parse_phg_gain("7"), do: {7, "7 dB"}
  defp parse_phg_gain("8"), do: {8, "8 dB"}
  defp parse_phg_gain("9"), do: {9, "9 dB"}
  defp parse_phg_gain(g), do: {nil, "Unknown gain: #{g}"}

  # PHG Directivity conversion (0-9)
  defp parse_phg_directivity("0"), do: {360, "Omni"}
  defp parse_phg_directivity("1"), do: {45, "45° NE"}
  defp parse_phg_directivity("2"), do: {90, "90° E"}
  defp parse_phg_directivity("3"), do: {135, "135° SE"}
  defp parse_phg_directivity("4"), do: {180, "180° S"}
  defp parse_phg_directivity("5"), do: {225, "225° SW"}
  defp parse_phg_directivity("6"), do: {270, "270° W"}
  defp parse_phg_directivity("7"), do: {315, "315° NW"}
  defp parse_phg_directivity("8"), do: {360, "360° N"}
  defp parse_phg_directivity("9"), do: {nil, "Undefined"}
  defp parse_phg_directivity(d), do: {nil, "Unknown directivity: #{d}"}

  # DF Strength conversion (0-9)
  defp parse_df_strength("0"), do: {0, "0 dB"}
  defp parse_df_strength("1"), do: {1, "3 dB above S0"}
  defp parse_df_strength("2"), do: {2, "6 dB above S0"}
  defp parse_df_strength("3"), do: {3, "9 dB above S0"}
  defp parse_df_strength("4"), do: {4, "12 dB above S0"}
  defp parse_df_strength("5"), do: {5, "15 dB above S0"}
  defp parse_df_strength("6"), do: {6, "18 dB above S0"}
  defp parse_df_strength("7"), do: {7, "21 dB above S0"}
  defp parse_df_strength("8"), do: {8, "24 dB above S0"}
  defp parse_df_strength("9"), do: {9, "27 dB above S0"}
  defp parse_df_strength(s), do: {nil, "Unknown strength: #{s}"}

  # PEET Logging parsing
  def parse_peet_logging(<<"*", peet_data::binary>>) do
    %{
      peet_data: peet_data,
      data_type: :peet_logging
    }
  end

  def parse_peet_logging(data) do
    %{
      peet_data: data,
      data_type: :peet_logging
    }
  end

  # Invalid/Test Data parsing
  def parse_invalid_test_data(<<",", test_data::binary>>) do
    %{
      test_data: test_data,
      data_type: :invalid_test_data
    }
  end

  def parse_invalid_test_data(data) do
    %{
      test_data: data,
      data_type: :invalid_test_data
    }
  end

  # Validation functions

  # Validate position data
  defp validate_position_data(latitude, longitude) do
    %{latitude: lat, longitude: lon} = Position.from_aprs(latitude, longitude)

    cond do
      not is_number(lat) or not is_number(lon) ->
        {:error, "Invalid coordinate format"}

      lat < -90 or lat > 90 ->
        {:error, "Latitude out of range"}

      lon < -180 or lon > 180 ->
        {:error, "Longitude out of range"}

      true ->
        {:ok, {lat, lon}}
    end
  rescue
    _ ->
      {:error, "Position parsing failed"}
  end

  # Validate timestamp format
  defp validate_timestamp(time) when byte_size(time) == 7 do
    if Regex.match?(~r/^\d{6}[hz\/]$/, time) do
      time
    else
      "INVALID"
    end
  end

  defp validate_timestamp(time), do: time
end

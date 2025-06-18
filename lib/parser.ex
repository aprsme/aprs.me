defmodule Parser do
  @moduledoc """
  Main parsing library
  """
  # import Bitwise
  alias Aprs.Convert
  alias Parser.Types.MicE
  alias Parser.Types.Position

  require Logger

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
  def parse(message) do
    with {:ok, [sender, path, data]} <- split_packet(message),
         {:ok, [base_callsign, ssid]} <- parse_callsign(sender),
         {:ok, data_type} <- parse_datatype_safe(data),
         {:ok, [destination, path]} <- split_path(path) do
      data_trimmed = String.trim(data)
      # Strip the first character (datatype indicator) from the data
      data_without_type = String.slice(data_trimmed, 1..-1//1)
      data_extended = parse_data(data_type, destination, data_without_type)

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
      # {:ok, file} = File.open("./badpackets.txt", [:append])
      # IO.binwrite(file, message <> "\n\n")
      # File.close(file)
      {:error, :invalid_packet}
  end

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
  def split_path(path) do
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
  @spec parse_datatype_safe(String.t()) :: {:ok, atom()} | {:error, String.t()}
  def parse_datatype_safe(""), do: {:error, "Empty data"}
  def parse_datatype_safe(data), do: {:ok, parse_datatype(data)}

  @spec parse_callsign(String.t()) :: {:ok, [String.t()]} | {:error, String.t()}
  def parse_callsign(callsign) do
    cond do
      not is_binary(callsign) ->
        {:error, "Invalid callsign format"}

      byte_size(callsign) == 0 ->
        {:error, "Empty callsign"}

      String.contains?(callsign, "-") ->
        case String.split(callsign, "-") do
          [base, ssid] -> {:ok, [base, ssid]}
          _ -> {:ok, [callsign, "0"]}
        end

      true ->
        {:ok, [callsign, "0"]}
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
  def parse_datatype(<<"!", _::binary>>), do: :position
  def parse_datatype(<<"/", _::binary>>), do: :timestamped_position
  def parse_datatype(<<"=", _::binary>>), do: :position_with_message
  def parse_datatype(<<"@", _::binary>>), do: :timestamped_position_with_message
  def parse_datatype(<<";", _::binary>>), do: :object
  def parse_datatype(<<"`", _::binary>>), do: :mic_e
  def parse_datatype(<<"'", _::binary>>), do: :mic_e_old
  def parse_datatype(<<"_", _::binary>>), do: :weather
  def parse_datatype(<<"T", _::binary>>), do: :telemetry
  def parse_datatype(<<"$", _::binary>>), do: :raw_gps_ultimeter
  def parse_datatype(<<"<", _::binary>>), do: :station_capabilities
  def parse_datatype(<<"?", _::binary>>), do: :query
  def parse_datatype(<<"{", _::binary>>), do: :user_defined
  def parse_datatype(<<"}", _::binary>>), do: :third_party_traffic
  def parse_datatype(<<"%", _::binary>>), do: :item
  def parse_datatype(<<")", _::binary>>), do: :item
  def parse_datatype(<<"*", _::binary>>), do: :peet_logging
  def parse_datatype(<<",", _::binary>>), do: :invalid_test_data
  def parse_datatype(<<"#", _::binary>>), do: :phg_data
  def parse_datatype(<<"(", _::binary>>), do: :unused
  def parse_datatype(<<"&", _::binary>>), do: :reserved
  def parse_datatype(_), do: :unknown_datatype

  @spec parse_data(atom(), String.t(), String.t()) :: map() | nil
  def parse_data(:mic_e, destination, data), do: parse_mic_e(destination, data)
  def parse_data(:mic_e_old, destination, data), do: parse_mic_e(destination, data)

  def parse_data(:position, _destination, data) do
    case data do
      <<"/", _::binary>> ->
        result = parse_position_without_timestamp(false, data)

        if result.data_type == :malformed_position do
          result
        else
          %{result | data_type: :position}
        end

      _ ->
        result = parse_position_without_timestamp(false, data)

        if result.data_type == :malformed_position do
          result
        else
          %{result | data_type: :position}
        end
    end
  end

  def parse_data(:position_with_message, _destination, data) do
    result = parse_position_without_timestamp(true, data)
    %{result | data_type: :position}
  end

  def parse_data(:timestamped_position, _destination, data) do
    case data do
      <<"/", _::binary>> ->
        %{
          data_type: :timestamped_position_error,
          error: "Compressed position not supported in timestamped position"
        }

      _ ->
        parse_position_with_timestamp(false, data, :timestamped_position)
    end
  end

  def parse_data(:timestamped_position_with_message, _destination, data) do
    case data do
      <<"/", _::binary>> ->
        %{
          data_type: :timestamped_position_error,
          error: "Compressed position not supported in timestamped position"
        }

      _ ->
        parse_position_with_timestamp(true, data)
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

  def parse_data(:raw_gps_ultimeter, _destination, data) do
    case parse_nmea_sentence(data) do
      {:ok, parsed_data} ->
        Map.put(parsed_data, :data_type, :raw_gps_ultimeter)

      {:error, reason} ->
        %{
          data_type: :raw_gps_ultimeter,
          error: reason,
          raw_data: data
        }
    end
  end

  def parse_data(_type, _destination, _data), do: nil

  @spec parse_position_with_datetime_and_weather(boolean(), String.t(), String.t()) :: map()
  def parse_position_with_datetime_and_weather(aprs_messaging?, date_time_position_data, weather_report) do
    case date_time_position_data do
      <<time::binary-size(7), latitude::binary-size(8), sym_table_id::binary-size(1), longitude::binary-size(9)>> ->
        %{latitude: lat, longitude: lon} = Position.from_aprs(latitude, longitude)

        weather_data = parse_weather_data(weather_report)

        %{
          latitude: lat,
          longitude: lon,
          timestamp: time,
          symbol_table_id: sym_table_id,
          symbol_code: "_",
          weather: weather_data,
          data_type: :position_with_datetime_and_weather,
          aprs_messaging?: aprs_messaging?
        }

      _ ->
        weather_data = parse_weather_data(weather_report)

        %{
          latitude: nil,
          longitude: nil,
          timestamp: nil,
          symbol_table_id: nil,
          symbol_code: nil,
          weather: weather_data,
          data_type: :position_with_datetime_and_weather,
          aprs_messaging?: aprs_messaging?
        }
    end
  end

  @spec decode_compressed_position(binary()) :: map()
  def decode_compressed_position(
        <<"/", latitude::binary-size(4), longitude::binary-size(4), symbol_code::binary-size(1), _cs::binary-size(2),
          _compression_type::binary-size(2), _rest::binary>>
      ) do
    lat = convert_to_base91(latitude)
    lon = convert_to_base91(longitude)

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

  @spec parse_position_without_timestamp(boolean(), String.t()) :: map()
  def parse_position_without_timestamp(aprs_messaging?, position_data) do
    case position_data do
      <<latitude::binary-size(8), sym_table_id::binary-size(1), longitude::binary-size(9), symbol_code::binary-size(1),
        comment::binary>> ->
        %{latitude: lat, longitude: lon} = Position.from_aprs(latitude, longitude)
        ambiguity = calculate_position_ambiguity(latitude, longitude)
        dao_data = parse_dao_extension(comment)

        base_data = %{
          latitude: lat,
          longitude: lon,
          timestamp: nil,
          symbol_table_id: sym_table_id,
          symbol_code: symbol_code,
          comment: comment,
          data_type: :position,
          aprs_messaging?: aprs_messaging?,
          compressed?: false,
          position_ambiguity: ambiguity
        }

        if dao_data, do: Map.put(base_data, :dao, dao_data), else: base_data

      <<latitude::binary-size(8), sym_table_id::binary-size(1), longitude::binary-size(9)>> ->
        %{latitude: lat, longitude: lon} = Position.from_aprs(latitude, longitude)
        ambiguity = calculate_position_ambiguity(latitude, longitude)

        %{
          latitude: lat,
          longitude: lon,
          timestamp: nil,
          symbol_table_id: sym_table_id,
          symbol_code: "_",
          data_type: :position,
          aprs_messaging?: aprs_messaging?,
          compressed?: false,
          position_ambiguity: ambiguity
        }

      <<"/", latitude_compressed::binary-size(4), longitude_compressed::binary-size(4), symbol_code::binary-size(1),
        cs::binary-size(2), compression_type::binary-size(1), comment::binary>> ->
        converted_lat = convert_compressed_lat(latitude_compressed)
        converted_lon = convert_compressed_lon(longitude_compressed)
        compressed_cs = convert_compressed_cs(cs)
        ambiguity = calculate_compressed_ambiguity(compression_type)

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
          position_ambiguity: ambiguity
        }

        Map.merge(base_data, compressed_cs)

      _ ->
        %{
          latitude: nil,
          longitude: nil,
          timestamp: nil,
          symbol_table_id: nil,
          symbol_code: nil,
          data_type: :malformed_position,
          aprs_messaging?: aprs_messaging?,
          compressed?: false,
          comment: String.trim(position_data)
        }
    end
  end

  @spec calculate_position_ambiguity(String.t(), String.t()) :: position_ambiguity()
  defp calculate_position_ambiguity(latitude, longitude) do
    # Count spaces in latitude and longitude
    lat_spaces = count_spaces(latitude)
    lon_spaces = count_spaces(longitude)

    # Position ambiguity is determined by the number of spaces
    # 0 = no ambiguity
    # 1 = 1/60th of a degree
    # 2 = 1/10th of a degree
    # 3 = 1 degree
    # 4 = 10 degrees
    cond do
      lat_spaces == 0 and lon_spaces == 0 -> 0
      lat_spaces == 1 and lon_spaces == 1 -> 1
      lat_spaces == 2 and lon_spaces == 2 -> 2
      lat_spaces == 3 and lon_spaces == 3 -> 3
      lat_spaces == 4 and lon_spaces == 4 -> 4
      true -> 0
    end
  end

  @spec calculate_compressed_ambiguity(binary()) :: position_ambiguity()
  defp calculate_compressed_ambiguity(compression_type) do
    case compression_type do
      # No ambiguity
      " " -> 0
      # 1/60th of a degree
      "!" -> 1
      # 1/10th of a degree
      "\"" -> 2
      # 1 degree
      "#" -> 3
      # 10 degrees
      "$" -> 4
      _ -> 0
    end
  end

  @spec count_spaces(String.t()) :: non_neg_integer()
  defp count_spaces(str) do
    str
    |> String.graphemes()
    |> Enum.count(fn c -> c == " " end)
  end

  # Add DAO (Datum) extension support
  @spec parse_dao_extension(String.t()) :: map() | nil
  defp parse_dao_extension(comment) do
    case Regex.run(~r/!([A-Za-z])([A-Za-z])([A-Za-z])!/, comment) do
      [_, lat_dao, lon_dao, _] ->
        %{
          lat_dao: lat_dao,
          lon_dao: lon_dao,
          # APRS uses WGS84 by default
          datum: "WGS84"
        }

      nil ->
        nil
    end
  end

  @spec parse_position_with_timestamp(boolean(), binary(), atom()) :: map()
  def parse_position_with_timestamp(
        aprs_messaging?,
        <<time::binary-size(7), latitude::binary-size(8), sym_table_id::binary-size(1), longitude::binary-size(9),
          symbol_code::binary-size(1), comment::binary>>,
        _data_type
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
          aprs_messaging?: aprs_messaging?,
          compressed?: false
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
          data_type: :timestamped_position_error,
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
        data_type: :timestamped_position_error,
        error: "Timestamped position parsing failed"
      }
  end

  @spec parse_position_with_timestamp(boolean(), binary()) :: map()
  def parse_position_with_timestamp(_aprs_messaging?, <<"/", _::binary>>) do
    %{
      data_type: :timestamped_position_error,
      error: "Compressed position not supported in timestamped position"
    }
  end

  def parse_position_with_timestamp(_aprs_messaging?, data) do
    %{
      data_type: :timestamped_position_error,
      error: "Invalid timestamped position format",
      raw_data: data
    }
  end

  @spec parse_mic_e(String.t(), String.t()) :: MicE.t() | map()
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

  @spec parse_mic_e(binary()) :: map()
  def parse_mic_e(data) do
    case data do
      <<dti::binary-size(1), latitude::binary-size(6), longitude::binary-size(7), symbol_code::binary-size(1),
        speed::binary-size(1), course::binary-size(1), symbol_table_id::binary-size(1), rest::binary>> ->
        %{latitude: lat, longitude: lon} = Position.from_aprs(latitude, longitude)

        %{
          latitude: lat,
          longitude: lon,
          symbol_table_id: symbol_table_id,
          symbol_code: symbol_code,
          speed: speed,
          course: course,
          dti: dti,
          comment: rest,
          data_type: :mic_e
        }

      _ ->
        nil
    end
  end

  @spec parse_mic_e_digit(binary()) :: [integer() | atom() | nil]
  def parse_mic_e_digit(<<c>>) when c in ?0..?9, do: [c - ?0, 0, nil]
  def parse_mic_e_digit(<<c>>) when c in ?A..?J, do: [c - ?A, 1, :custom]
  def parse_mic_e_digit(<<c>>) when c in ?P..?Y, do: [c - ?P, 1, :standard]
  def parse_mic_e_digit(<<"K">>), do: [0, 1, :custom]
  def parse_mic_e_digit(<<"L">>), do: [0, 0, nil]
  def parse_mic_e_digit(<<"Z">>), do: [0, 1, :standard]
  def parse_mic_e_digit(_), do: [:unknown, :unknown, :unknown]

  @spec parse_mic_e_destination(String.t()) :: map()
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

    manufacturer = parse_manufacturer(symbol1 <> result["secondtolast"] <> result["last"])

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

  @spec parse_manufacturer(binary()) :: String.t()
  def parse_manufacturer(symbols) do
    Aprs.DeviceIdentification.identify_device(symbols)
  end

  @spec find_matches(Regex.t(), String.t()) :: [String.t()]
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
    timestamp = extract_timestamp(weather_data)
    weather_data = remove_timestamp(weather_data)

    # Parse each weather component
    weather_values = %{
      wind_direction: parse_wind_direction(weather_data),
      wind_speed: parse_wind_speed(weather_data),
      wind_gust: parse_wind_gust(weather_data),
      temperature: parse_temperature(weather_data),
      rain_1h: parse_rainfall_1h(weather_data),
      rain_24h: parse_rainfall_24h(weather_data),
      rain_since_midnight: parse_rainfall_since_midnight(weather_data),
      humidity: parse_humidity(weather_data),
      pressure: parse_pressure(weather_data),
      luminosity: parse_luminosity(weather_data),
      snow: parse_snow(weather_data)
    }

    # Build base result map
    result = %{
      timestamp: timestamp,
      data_type: :weather,
      raw_weather_data: weather_data
    }

    # Add non-nil weather values to result
    Enum.reduce(weather_values, result, fn {key, value}, acc ->
      if value == nil, do: acc, else: Map.put(acc, key, value)
    end)
  end

  defp parse_temperature(weather_data) do
    case Regex.run(~r/t(-?\d{3})/, weather_data) do
      [_, temp] -> String.to_integer(temp)
      nil -> nil
    end
  end

  defp parse_wind_direction(weather_data) do
    case Regex.run(~r/(\d{3})\//, weather_data) do
      [_, direction] -> String.to_integer(direction)
      nil -> nil
    end
  end

  defp parse_wind_speed(weather_data) do
    case Regex.run(~r/\/(\d{3})/, weather_data) do
      [_, speed] -> String.to_integer(speed)
      nil -> nil
    end
  end

  defp parse_wind_gust(weather_data) do
    case Regex.run(~r/g(\d{3})/, weather_data) do
      [_, gust] -> String.to_integer(gust)
      nil -> nil
    end
  end

  defp parse_rainfall_1h(weather_data) do
    case Regex.run(~r/r(\d{3})/, weather_data) do
      [_, rain] -> String.to_integer(rain)
      nil -> nil
    end
  end

  defp parse_rainfall_24h(weather_data) do
    case Regex.run(~r/p(\d{3})/, weather_data) do
      [_, rain] -> String.to_integer(rain)
      nil -> nil
    end
  end

  defp parse_rainfall_since_midnight(weather_data) do
    case Regex.run(~r/P(\d{3})/, weather_data) do
      [_, rain] -> String.to_integer(rain)
      nil -> nil
    end
  end

  defp parse_humidity(weather_data) do
    case Regex.run(~r/h(\d{2})/, weather_data) do
      [_, humidity] ->
        val = String.to_integer(humidity)
        if val == 0, do: 100, else: val

      nil ->
        nil
    end
  end

  defp parse_pressure(weather_data) do
    case Regex.run(~r/b(\d{5})/, weather_data) do
      [_, pressure] -> String.to_integer(pressure) / 10.0
      nil -> nil
    end
  end

  defp parse_luminosity(weather_data) do
    case Regex.run(~r/[lL](\d{3})/, weather_data) do
      [_, luminosity] -> String.to_integer(luminosity)
      nil -> nil
    end
  end

  defp parse_snow(weather_data) do
    case Regex.run(~r/s(\d{3})/, weather_data) do
      [_, snow] -> String.to_integer(snow)
      nil -> nil
    end
  end

  # Telemetry parsing
  def parse_telemetry(<<"T#", rest::binary>>) do
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
          a: parse_coefficient(a),
          b: parse_coefficient(b),
          c: parse_coefficient(c)
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

  # Parse telemetry sequence number
  @spec parse_telemetry_sequence(String.t()) :: integer() | nil
  defp parse_telemetry_sequence(seq) do
    case Integer.parse(seq) do
      {num, _} -> num
      :error -> nil
    end
  end

  # Parse digital values (convert to integers where possible)
  @spec parse_digital_values([String.t()]) :: [boolean() | nil]
  defp parse_digital_values(values) do
    values
    |> Enum.map(fn value ->
      case value do
        "0" ->
          false

        "1" ->
          true

        binary when is_binary(binary) ->
          # Handle binary string format (e.g., "00000000")
          binary
          |> String.graphemes()
          |> Enum.map(fn
            "0" -> false
            "1" -> true
            _ -> nil
          end)

        _ ->
          nil
      end
    end)
    |> List.flatten()
  end

  # Parse analog values (convert to floats where possible)
  @spec parse_analog_values([String.t()]) :: [float() | nil]
  defp parse_analog_values(values) do
    Enum.map(values, fn value ->
      case value do
        "" ->
          nil

        value ->
          case Float.parse(value) do
            {float_val, _} -> float_val
            :error -> nil
          end
      end
    end)
  end

  # Parse equation coefficient
  @spec parse_coefficient(String.t()) :: float() | integer() | String.t()
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
  @spec parse_third_party_traffic(String.t()) :: map()
  def parse_third_party_traffic(<<"}", third_party_packet::binary>>) do
    # Third party traffic contains a complete APRS packet
    case parse_tunneled_packet(third_party_packet) do
      {:ok, parsed_packet} ->
        # Check for nested tunnels
        case parse_nested_tunnel(third_party_packet) do
          {:ok, nested_packet} ->
            %{
              third_party_packet: nested_packet,
              data_type: :third_party_traffic,
              raw_data: third_party_packet
            }

          {:error, _} ->
            %{
              third_party_packet: parsed_packet,
              data_type: :third_party_traffic,
              raw_data: third_party_packet
            }
        end

      {:error, reason} ->
        %{
          third_party_data: third_party_packet,
          data_type: :third_party_traffic,
          error: reason
        }
    end
  end

  @spec parse_third_party_traffic(String.t()) :: map()
  def parse_third_party_traffic(data) do
    %{
      third_party_data: data,
      data_type: :third_party_traffic
    }
  end

  @spec parse_tunneled_packet(String.t()) :: {:ok, map()} | {:error, String.t()}
  defp parse_tunneled_packet(packet) do
    # Split the packet into header and information field
    case String.split(packet, ":", parts: 2) do
      [header, information] ->
        # Parse the header (source>destination,digipeaters)
        case parse_tunneled_header(header) do
          {:ok, header_data} ->
            # Parse the information field based on its type
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

          {:error, reason} ->
            {:error, "Invalid header: #{reason}"}
        end

      _ ->
        {:error, "Invalid tunneled packet format"}
    end
  end

  @spec parse_tunneled_header(String.t()) :: {:ok, map()} | {:error, String.t()}
  defp parse_tunneled_header(header) do
    case String.split(header, ">", parts: 2) do
      [sender, path] ->
        case parse_callsign(sender) do
          {:ok, [base_callsign, ssid]} ->
            case split_path(path) do
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
  @spec parse_nested_tunnel(String.t(), non_neg_integer()) :: {:ok, map()} | {:error, String.t()}
  defp parse_nested_tunnel(packet, depth \\ 0) do
    cond do
      depth > 3 ->
        {:error, "Maximum tunnel depth exceeded"}

      String.starts_with?(packet, "}") ->
        case parse_network_tunnel(packet) do
          {:ok, parsed_packet} ->
            # Recursively parse any nested tunnels
            case Map.get(parsed_packet, :data_extended) do
              %{raw_data: nested_data} when is_binary(nested_data) ->
                case parse_nested_tunnel(nested_data, depth + 1) do
                  {:ok, nested_packet} ->
                    {:ok, Map.put(parsed_packet, :nested_packet, nested_packet)}

                  {:error, _} ->
                    {:ok, parsed_packet}
                end

              _ ->
                {:ok, parsed_packet}
            end

          {:error, reason} ->
            {:error, reason}
        end

      true ->
        {:error, "Not a tunneled packet"}
    end
  end

  # PHG Data parsing (Power, Height, Gain, Directivity)
  def parse_phg_data(<<"#PHG", p, h, g, d, rest::binary>>) do
    %{
      power: parse_phg_power(p),
      height: parse_phg_height(h),
      gain: parse_phg_gain(g),
      directivity: parse_phg_directivity(d),
      comment: rest,
      data_type: :phg_data
    }
  end

  def parse_phg_data(<<"#DFS", s, h, g, d, rest::binary>>) do
    %{
      df_strength: parse_df_strength(s),
      height: parse_phg_height(h),
      gain: parse_phg_gain(g),
      directivity: parse_phg_directivity(d),
      comment: rest,
      data_type: :df_report
    }
  end

  def parse_phg_data(<<"#", phg_data::binary>>) do
    %{
      phg_data: phg_data,
      data_type: :phg_data
    }
  end

  @spec parse_phg_data(String.t()) :: map()
  def parse_phg_data(data) do
    %{
      phg_data: data,
      data_type: :phg_data
    }
  end

  # PHG Power conversion (0-9)
  defp parse_phg_power(?0), do: {0, "0 watts"}
  defp parse_phg_power(?1), do: {1, "1 watt"}
  defp parse_phg_power(?2), do: {4, "4 watts"}
  defp parse_phg_power(?3), do: {9, "9 watts"}
  defp parse_phg_power(?4), do: {16, "16 watts"}
  defp parse_phg_power(?5), do: {25, "25 watts"}
  defp parse_phg_power(?6), do: {36, "36 watts"}
  defp parse_phg_power(?7), do: {49, "49 watts"}
  defp parse_phg_power(?8), do: {64, "64 watts"}
  defp parse_phg_power(?9), do: {81, "81 watts"}
  defp parse_phg_power(p), do: {nil, "Unknown power: #{<<p>>}"}

  # PHG Height conversion (0-9)
  defp parse_phg_height(?0), do: {10, "10 feet"}
  defp parse_phg_height(?1), do: {20, "20 feet"}
  defp parse_phg_height(?2), do: {40, "40 feet"}
  defp parse_phg_height(?3), do: {80, "80 feet"}
  defp parse_phg_height(?4), do: {160, "160 feet"}
  defp parse_phg_height(?5), do: {320, "320 feet"}
  defp parse_phg_height(?6), do: {640, "640 feet"}
  defp parse_phg_height(?7), do: {1280, "1280 feet"}
  defp parse_phg_height(?8), do: {2560, "2560 feet"}
  defp parse_phg_height(?9), do: {5120, "5120 feet"}
  defp parse_phg_height(h), do: {nil, "Unknown height: #{<<h>>}"}

  # PHG Gain conversion (0-9)
  defp parse_phg_gain(?0), do: {0, "0 dB"}
  defp parse_phg_gain(?1), do: {1, "1 dB"}
  defp parse_phg_gain(?2), do: {2, "2 dB"}
  defp parse_phg_gain(?3), do: {3, "3 dB"}
  defp parse_phg_gain(?4), do: {4, "4 dB"}
  defp parse_phg_gain(?5), do: {5, "5 dB"}
  defp parse_phg_gain(?6), do: {6, "6 dB"}
  defp parse_phg_gain(?7), do: {7, "7 dB"}
  defp parse_phg_gain(?8), do: {8, "8 dB"}
  defp parse_phg_gain(?9), do: {9, "9 dB"}
  defp parse_phg_gain(g), do: {nil, "Unknown gain: #{<<g>>}"}

  # PHG Directivity conversion (0-9)
  defp parse_phg_directivity(?0), do: {360, "Omni"}
  defp parse_phg_directivity(?1), do: {45, "45° NE"}
  defp parse_phg_directivity(?2), do: {90, "90° E"}
  defp parse_phg_directivity(?3), do: {135, "135° SE"}
  defp parse_phg_directivity(?4), do: {180, "180° S"}
  defp parse_phg_directivity(?5), do: {225, "225° SW"}
  defp parse_phg_directivity(?6), do: {270, "270° W"}
  defp parse_phg_directivity(?7), do: {315, "315° NW"}
  defp parse_phg_directivity(?8), do: {360, "360° N"}
  defp parse_phg_directivity(?9), do: {nil, "Undefined"}
  defp parse_phg_directivity(d), do: {nil, "Unknown directivity: #{<<d>>}"}

  # DF Strength conversion (0-9)
  defp parse_df_strength(?0), do: {0, "0 dB"}
  defp parse_df_strength(?1), do: {1, "3 dB above S0"}
  defp parse_df_strength(?2), do: {2, "6 dB above S0"}
  defp parse_df_strength(?3), do: {3, "9 dB above S0"}
  defp parse_df_strength(?4), do: {4, "12 dB above S0"}
  defp parse_df_strength(?5), do: {5, "15 dB above S0"}
  defp parse_df_strength(?6), do: {6, "18 dB above S0"}
  defp parse_df_strength(?7), do: {7, "21 dB above S0"}
  defp parse_df_strength(?8), do: {8, "24 dB above S0"}
  defp parse_df_strength(?9), do: {9, "27 dB above S0"}
  defp parse_df_strength(s), do: {nil, "Unknown strength: #{<<s>>}"}

  # PEET Logging parsing
  def parse_peet_logging(<<"*", peet_data::binary>>) do
    %{
      peet_data: peet_data,
      data_type: :peet_logging
    }
  end

  @spec parse_peet_logging(String.t()) :: map()
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

  @spec parse_invalid_test_data(String.t()) :: map()
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
  @spec validate_timestamp(String.t()) :: String.t() | nil
  defp validate_timestamp(time) when byte_size(time) == 7 do
    if Regex.match?(~r/^\d{6}[hz\/]$/, time) do
      time
    else
      "INVALID"
    end
  end

  @spec validate_timestamp(any()) :: nil
  defp validate_timestamp(_), do: nil

  @spec extract_timestamp(String.t()) :: String.t() | nil
  defp extract_timestamp(weather_data) do
    case Regex.run(~r/^(\d{6}[hz\/])/, weather_data) do
      [_, timestamp] -> timestamp
      nil -> nil
    end
  end

  @spec remove_timestamp(String.t()) :: String.t()
  defp remove_timestamp(weather_data) do
    case Regex.run(~r/^\d{6}[hz\/]/, weather_data) do
      [timestamp] -> String.replace(weather_data, timestamp, "")
      nil -> weather_data
    end
  end

  @spec parse_nmea_sentence(String.t()) :: {:ok, map()} | {:error, String.t()}
  defp parse_nmea_sentence(sentence) do
    # Validate NMEA sentence format
    with {:ok, sentence_type} <- validate_nmea_sentence(sentence),
         {:ok, fields} <- split_nmea_fields(sentence),
         {:ok, _checksum} <- validate_nmea_checksum(sentence) do
      parse_nmea_by_type(sentence_type, fields)
    end
  end

  @spec validate_nmea_sentence(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  defp validate_nmea_sentence(sentence) do
    case Regex.run(~r/^\$([A-Z]{5}),/, sentence) do
      [_, sentence_type] -> {:ok, sentence_type}
      nil -> {:error, "Invalid NMEA sentence format"}
    end
  end

  @spec split_nmea_fields(String.t()) :: {:ok, [String.t()]} | {:error, String.t()}
  defp split_nmea_fields(sentence) do
    # Remove $ and checksum, then split by comma
    case String.split(sentence, ["$", "*"], parts: 2) do
      [_, fields] ->
        {:ok, String.split(fields, ",")}

      _ ->
        {:error, "Invalid NMEA sentence format"}
    end
  end

  @spec validate_nmea_checksum(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  defp validate_nmea_checksum(sentence) do
    case Regex.run(~r/\*([0-9A-F]{2})$/, sentence) do
      [_, checksum] ->
        calculated = calculate_nmea_checksum(sentence)

        if calculated == checksum do
          {:ok, checksum}
        else
          {:error, "Invalid NMEA checksum"}
        end

      nil ->
        {:error, "Missing NMEA checksum"}
    end
  end

  @spec calculate_nmea_checksum(String.t()) :: String.t()
  defp calculate_nmea_checksum(sentence) do
    # Remove $ and everything after *
    sentence
    |> String.split("*")
    |> List.first()
    |> String.slice(1..-1//-1)
    |> String.to_charlist()
    |> Enum.reduce(0, &Bitwise.bxor/2)
    |> Integer.to_string(16)
    |> String.pad_leading(2, "0")
    |> String.upcase()
  end

  @spec parse_nmea_by_type(String.t(), [String.t()]) :: {:ok, map()} | {:error, String.t()}
  defp parse_nmea_by_type("GPGGA", fields) do
    with {:ok, time} <- parse_nmea_time(Enum.at(fields, 1)),
         {:ok, lat} <- parse_nmea_coordinate(Enum.at(fields, 2), Enum.at(fields, 3)),
         {:ok, lon} <- parse_nmea_coordinate(Enum.at(fields, 4), Enum.at(fields, 5)),
         {:ok, quality} <- parse_nmea_quality(Enum.at(fields, 6)),
         {:ok, satellites} <- parse_nmea_satellites(Enum.at(fields, 7)),
         {:ok, hdop} <- parse_nmea_hdop(Enum.at(fields, 8)),
         {:ok, altitude} <- parse_nmea_altitude(Enum.at(fields, 9), Enum.at(fields, 10)) do
      {:ok,
       %{
         time: time,
         latitude: lat,
         longitude: lon,
         quality: quality,
         satellites: satellites,
         hdop: hdop,
         altitude: altitude,
         nmea_type: "GPGGA"
       }}
    end
  end

  defp parse_nmea_by_type("GPRMC", fields) do
    with {:ok, time} <- parse_nmea_time(Enum.at(fields, 1)),
         {:ok, status} <- parse_nmea_status(Enum.at(fields, 2)),
         {:ok, lat} <- parse_nmea_coordinate(Enum.at(fields, 3), Enum.at(fields, 4)),
         {:ok, lon} <- parse_nmea_coordinate(Enum.at(fields, 5), Enum.at(fields, 6)),
         {:ok, speed} <- parse_nmea_speed(Enum.at(fields, 7)),
         {:ok, course} <- parse_nmea_course(Enum.at(fields, 8)),
         {:ok, date} <- parse_nmea_date(Enum.at(fields, 9)) do
      {:ok,
       %{
         time: time,
         status: status,
         latitude: lat,
         longitude: lon,
         speed: speed,
         course: course,
         date: date,
         nmea_type: "GPRMC"
       }}
    end
  end

  defp parse_nmea_by_type("GPGLL", fields) do
    with {:ok, lat} <- parse_nmea_coordinate(Enum.at(fields, 1), Enum.at(fields, 2)),
         {:ok, lon} <- parse_nmea_coordinate(Enum.at(fields, 3), Enum.at(fields, 4)),
         {:ok, time} <- parse_nmea_time(Enum.at(fields, 5)),
         {:ok, status} <- parse_nmea_status(Enum.at(fields, 6)) do
      {:ok,
       %{
         latitude: lat,
         longitude: lon,
         time: time,
         status: status,
         nmea_type: "GPGLL"
       }}
    end
  end

  defp parse_nmea_by_type(type, _fields) do
    {:error, "Unsupported NMEA sentence type: #{type}"}
  end

  @spec parse_nmea_time(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  defp parse_nmea_time(time) when is_binary(time) and byte_size(time) == 10 do
    {:ok, time}
  end

  defp parse_nmea_time(_), do: {:error, "Invalid NMEA time format"}

  @spec parse_nmea_coordinate(String.t(), String.t()) :: {:ok, float()} | {:error, String.t()}
  defp parse_nmea_coordinate(value, direction) when is_binary(value) and is_binary(direction) do
    case Float.parse(value) do
      {coord, _} ->
        coord = coord / 100.0

        coord =
          case direction do
            "N" -> coord
            "S" -> -coord
            "E" -> coord
            "W" -> -coord
            _ -> {:error, "Invalid coordinate direction"}
          end

        {:ok, coord}

      :error ->
        {:error, "Invalid coordinate value"}
    end
  end

  defp parse_nmea_coordinate(_, _), do: {:error, "Invalid coordinate format"}

  @spec parse_nmea_quality(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  defp parse_nmea_quality("0"), do: {:ok, "Invalid"}
  defp parse_nmea_quality("1"), do: {:ok, "GPS fix"}
  defp parse_nmea_quality("2"), do: {:ok, "DGPS fix"}
  defp parse_nmea_quality("4"), do: {:ok, "RTK fix"}
  defp parse_nmea_quality("5"), do: {:ok, "RTK float"}
  defp parse_nmea_quality(_), do: {:error, "Invalid quality indicator"}

  @spec parse_nmea_satellites(String.t()) :: {:ok, integer()} | {:error, String.t()}
  defp parse_nmea_satellites(satellites) do
    case Integer.parse(satellites) do
      {num, _} -> {:ok, num}
      :error -> {:error, "Invalid satellite count"}
    end
  end

  @spec parse_nmea_hdop(String.t()) :: {:ok, float()} | {:error, String.t()}
  defp parse_nmea_hdop(hdop) do
    case Float.parse(hdop) do
      {num, _} -> {:ok, num}
      :error -> {:error, "Invalid HDOP value"}
    end
  end

  @spec parse_nmea_altitude(String.t(), String.t()) :: {:ok, float()} | {:error, String.t()}
  defp parse_nmea_altitude(altitude, unit) do
    case Float.parse(altitude) do
      {num, _} ->
        num =
          case unit do
            "M" -> num
            # Convert feet to meters
            "F" -> num * 0.3048
            _ -> {:error, "Invalid altitude unit"}
          end

        {:ok, num}

      :error ->
        {:error, "Invalid altitude value"}
    end
  end

  @spec parse_nmea_status(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  defp parse_nmea_status("A"), do: {:ok, "Valid"}
  defp parse_nmea_status("V"), do: {:ok, "Invalid"}
  defp parse_nmea_status(_), do: {:error, "Invalid status"}

  @spec parse_nmea_speed(String.t()) :: {:ok, float()} | {:error, String.t()}
  defp parse_nmea_speed(speed) do
    case Float.parse(speed) do
      # Convert knots to km/h
      {num, _} -> {:ok, num * 1.852}
      :error -> {:error, "Invalid speed value"}
    end
  end

  @spec parse_nmea_course(String.t()) :: {:ok, float()} | {:error, String.t()}
  defp parse_nmea_course(course) do
    case Float.parse(course) do
      {num, _} -> {:ok, num}
      :error -> {:error, "Invalid course value"}
    end
  end

  @spec parse_nmea_date(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  defp parse_nmea_date(date) when is_binary(date) and byte_size(date) == 6 do
    {:ok, date}
  end

  defp parse_nmea_date(_), do: {:error, "Invalid NMEA date format"}
end

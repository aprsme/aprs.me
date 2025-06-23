defmodule Parser.Helpers do
  @moduledoc """
  Public helper functions for APRS parsing (NMEA, PHG/DF, compressed position, etc).
  """

  import Decimal, only: [new: 1, add: 2, negate: 1]

  @type phg_power :: {integer() | nil, String.t()}
  @type phg_height :: {integer() | nil, String.t()}
  @type phg_gain :: {integer() | nil, String.t()}
  @type phg_directivity :: {integer() | nil, String.t()}
  @type df_strength :: {integer() | nil, String.t()}

  # NMEA helpers
  @spec parse_nmea_coordinate(String.t(), String.t()) :: {:ok, float()} | {:error, String.t()}
  def parse_nmea_coordinate(value, direction) when is_binary(value) and is_binary(direction) do
    case Float.parse(value) do
      {coord, _} ->
        coord = coord / 100.0
        coord = apply_nmea_direction(coord, direction)
        handle_coordinate_result(coord)

      :error ->
        {:error, "Invalid coordinate value"}
    end
  end

  @spec parse_nmea_coordinate(any(), any()) :: {:error, String.t()}
  def parse_nmea_coordinate(_, _), do: {:error, "Invalid coordinate format"}

  defp handle_coordinate_result(coord) when is_tuple(coord), do: coord
  defp handle_coordinate_result(coord), do: {:ok, coord}

  defp apply_nmea_direction(coord, direction) do
    case direction do
      "N" -> coord
      "S" -> -coord
      "E" -> coord
      "W" -> -coord
      _ -> {:error, "Invalid coordinate direction"}
    end
  end

  # PHG/DF helpers
  @spec parse_phg_power(integer()) :: phg_power
  def parse_phg_power(?0), do: {1, "1 watt"}
  def parse_phg_power(?1), do: {4, "4 watts"}
  def parse_phg_power(?2), do: {9, "9 watts"}
  def parse_phg_power(?3), do: {16, "16 watts"}
  def parse_phg_power(?4), do: {25, "25 watts"}
  def parse_phg_power(?5), do: {36, "36 watts"}
  def parse_phg_power(?6), do: {49, "49 watts"}
  def parse_phg_power(?7), do: {64, "64 watts"}
  def parse_phg_power(?8), do: {81, "81 watts"}
  def parse_phg_power(?9), do: {81, "81 watts"}
  def parse_phg_power(p), do: {nil, "Unknown power: #{<<p>>}"}

  @spec parse_phg_height(integer()) :: phg_height
  def parse_phg_height(?0), do: {10, "10 feet"}
  def parse_phg_height(?1), do: {20, "20 feet"}
  def parse_phg_height(?2), do: {40, "40 feet"}
  def parse_phg_height(?3), do: {80, "80 feet"}
  def parse_phg_height(?4), do: {160, "160 feet"}
  def parse_phg_height(?5), do: {320, "320 feet"}
  def parse_phg_height(?6), do: {640, "640 feet"}
  def parse_phg_height(?7), do: {1280, "1280 feet"}
  def parse_phg_height(?8), do: {2560, "2560 feet"}
  def parse_phg_height(?9), do: {5120, "5120 feet"}
  def parse_phg_height(h), do: {nil, "Unknown height: #{<<h>>}"}

  @spec parse_phg_gain(integer()) :: phg_gain
  def parse_phg_gain(?0), do: {0, "0 dB"}
  def parse_phg_gain(?1), do: {1, "1 dB"}
  def parse_phg_gain(?2), do: {2, "2 dB"}
  def parse_phg_gain(?3), do: {3, "3 dB"}
  def parse_phg_gain(?4), do: {4, "4 dB"}
  def parse_phg_gain(?5), do: {5, "5 dB"}
  def parse_phg_gain(?6), do: {6, "6 dB"}
  def parse_phg_gain(?7), do: {7, "7 dB"}
  def parse_phg_gain(?8), do: {8, "8 dB"}
  def parse_phg_gain(?9), do: {9, "9 dB"}
  def parse_phg_gain(g), do: {nil, "Unknown gain: #{<<g>>}"}

  @spec parse_phg_directivity(integer()) :: phg_directivity
  def parse_phg_directivity(?0), do: {360, "Omni"}
  def parse_phg_directivity(?1), do: {45, "45° NE"}
  def parse_phg_directivity(?2), do: {90, "90° E"}
  def parse_phg_directivity(?3), do: {135, "135° SE"}
  def parse_phg_directivity(?4), do: {180, "180° S"}
  def parse_phg_directivity(?5), do: {225, "225° SW"}
  def parse_phg_directivity(?6), do: {270, "270° W"}
  def parse_phg_directivity(?7), do: {315, "315° NW"}
  def parse_phg_directivity(?8), do: {360, "360° N"}
  def parse_phg_directivity(?9), do: {nil, "Undefined"}
  def parse_phg_directivity(d), do: {nil, "Unknown directivity: #{<<d>>}"}

  @spec parse_df_strength(integer()) :: df_strength
  def parse_df_strength(?0), do: {0, "0 dB"}
  def parse_df_strength(?1), do: {1, "3 dB above S0"}
  def parse_df_strength(?2), do: {2, "6 dB above S0"}
  def parse_df_strength(?3), do: {3, "9 dB above S0"}
  def parse_df_strength(?4), do: {4, "12 dB above S0"}
  def parse_df_strength(?5), do: {5, "15 dB above S0"}
  def parse_df_strength(?6), do: {6, "18 dB above S0"}
  def parse_df_strength(?7), do: {7, "21 dB above S0"}
  def parse_df_strength(?8), do: {8, "24 dB above S0"}
  def parse_df_strength(?9), do: {9, "27 dB above S0"}
  def parse_df_strength(s), do: {nil, "Unknown strength: #{<<s>>}"}

  # Compressed position helpers
  @spec convert_compressed_lat(binary()) :: {:ok, float()} | {:error, String.t()}
  def convert_compressed_lat(lat) when is_binary(lat) and byte_size(lat) == 4 do
    [l1, l2, l3, l4] = to_charlist(lat)
    {:ok, 90 - ((l1 - 33) * 91 ** 3 + (l2 - 33) * 91 ** 2 + (l3 - 33) * 91 + l4 - 33) / 380_926}
  end

  def convert_compressed_lat(_), do: {:error, "Invalid compressed latitude"}

  @spec convert_compressed_lon(binary()) :: {:ok, float()} | {:error, String.t()}
  def convert_compressed_lon(lon) when is_binary(lon) and byte_size(lon) == 4 do
    [l1, l2, l3, l4] = to_charlist(lon)
    {:ok, -180 + ((l1 - 33) * 91 ** 3 + (l2 - 33) * 91 ** 2 + (l3 - 33) * 91 + l4 - 33) / 190_463}
  end

  def convert_compressed_lon(_), do: {:error, "Invalid compressed longitude"}

  # PEET Logging parsing
  @spec parse_peet_logging(binary()) :: map()
  def parse_peet_logging(<<"*", peet_data::binary>>) do
    %{
      peet_data: peet_data,
      data_type: :peet_logging
    }
  end

  @spec parse_peet_logging(any()) :: map()
  def parse_peet_logging(data) do
    %{
      peet_data: data,
      data_type: :peet_logging
    }
  end

  # Invalid/Test Data parsing
  @spec parse_invalid_test_data(binary()) :: map()
  def parse_invalid_test_data(<<",", test_data::binary>>) do
    %{
      test_data: test_data,
      data_type: :invalid_test_data
    }
  end

  @spec parse_invalid_test_data(any()) :: map()
  def parse_invalid_test_data(data) do
    %{
      test_data: data,
      data_type: :invalid_test_data
    }
  end

  # NMEA sentence parsing (minimal public wrapper)
  @spec parse_nmea_sentence(any()) :: {:error, String.t()}
  def parse_nmea_sentence(_sentence) do
    # This is a minimal wrapper; full NMEA parsing logic can be implemented as needed
    {:error, "NMEA parsing not implemented"}
  end

  # KISS to TNC2 conversion
  @spec kiss_to_tnc2(binary()) :: binary()
  def kiss_to_tnc2(<<0xC0, 0x00, rest::binary>>) do
    # Remove KISS framing and control byte
    tnc2 =
      rest
      |> String.trim_trailing(<<0xC0>>)
      |> String.replace(<<0xDB, 0xDC>>, <<0xC0>>)
      |> String.replace(<<0xDB, 0xDD>>, <<0xDB>>)

    tnc2
  end

  @spec kiss_to_tnc2(any()) :: map()
  def kiss_to_tnc2(_), do: %{error_code: :packet_invalid, error_message: "Unknown error"}

  # TNC2 to KISS conversion
  @spec tnc2_to_kiss(binary()) :: binary()
  def tnc2_to_kiss(tnc2) do
    # Add KISS framing and escape special bytes
    escaped =
      tnc2
      |> String.replace(<<0xDB>>, <<0xDB, 0xDD>>)
      |> String.replace(<<0xC0>>, <<0xDB, 0xDC>>)

    <<0xC0, 0x00>> <> escaped <> <<0xC0>>
  end

  # Telemetry helpers
  @spec parse_telemetry_sequence(String.t()) :: integer() | nil
  def parse_telemetry_sequence(seq) do
    case Integer.parse(seq) do
      {num, _} -> num
      :error -> nil
    end
  end

  def parse_analog_values(values) do
    Enum.map(values, &parse_analog_value/1)
  end

  defp parse_analog_value("") do
    nil
  end

  defp parse_analog_value(value) do
    case Float.parse(value) do
      {float_val, _} -> float_val
      :error -> nil
    end
  end

  def parse_digital_values(values) do
    Enum.flat_map(values, &parse_digital_value/1)
  end

  defp parse_digital_value("1"), do: [true]

  defp parse_digital_value(value) when is_binary(value) do
    value
    |> String.graphemes()
    |> Enum.map(fn
      "1" -> true
      "0" -> false
      _ -> nil
    end)
  end

  defp parse_digital_value(_), do: [nil]

  def parse_coefficient(coeff) do
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

  # Weather helpers
  @spec extract_timestamp(String.t()) :: String.t() | nil
  def extract_timestamp(weather_data) do
    case Regex.run(~r/^(\d{6}[hz\/])/, weather_data) do
      [_, timestamp] -> timestamp
      nil -> nil
    end
  end

  @spec remove_timestamp(String.t()) :: String.t()
  def remove_timestamp(weather_data) do
    case Regex.run(~r/^\d{6}[hz\/]/, weather_data) do
      [timestamp] -> String.replace(weather_data, timestamp, "")
      nil -> weather_data
    end
  end

  @spec parse_wind_direction(String.t()) :: integer() | nil
  def parse_wind_direction(weather_data) do
    case Regex.run(~r/(\d{3})\//, weather_data) do
      [_, direction] -> String.to_integer(direction)
      nil -> nil
    end
  end

  @spec parse_wind_speed(String.t()) :: integer() | nil
  def parse_wind_speed(weather_data) do
    case Regex.run(~r/\/(\d{3})/, weather_data) do
      [_, speed] -> String.to_integer(speed)
      nil -> nil
    end
  end

  @spec parse_wind_gust(String.t()) :: integer() | nil
  def parse_wind_gust(weather_data) do
    case Regex.run(~r/g(\d{3})/, weather_data) do
      [_, gust] -> String.to_integer(gust)
      nil -> nil
    end
  end

  @spec parse_temperature(String.t()) :: integer() | nil
  def parse_temperature(weather_data) do
    case Regex.run(~r/t(-?\d{3})/, weather_data) do
      [_, temp] -> String.to_integer(temp)
      nil -> nil
    end
  end

  @spec parse_rainfall_1h(String.t()) :: integer() | nil
  def parse_rainfall_1h(weather_data) do
    case Regex.run(~r/r(\d{3})/, weather_data) do
      [_, rain] -> String.to_integer(rain)
      nil -> nil
    end
  end

  @spec parse_rainfall_24h(String.t()) :: integer() | nil
  def parse_rainfall_24h(weather_data) do
    case Regex.run(~r/p(\d{3})/, weather_data) do
      [_, rain] -> String.to_integer(rain)
      nil -> nil
    end
  end

  @spec parse_rainfall_since_midnight(String.t()) :: integer() | nil
  def parse_rainfall_since_midnight(weather_data) do
    case Regex.run(~r/P(\d{3})/, weather_data) do
      [_, rain] -> String.to_integer(rain)
      nil -> nil
    end
  end

  @spec parse_humidity(String.t()) :: integer() | nil
  def parse_humidity(weather_data) do
    case Regex.run(~r/h(\d{2})/, weather_data) do
      [_, humidity] ->
        val = String.to_integer(humidity)
        normalize_humidity(val)

      nil ->
        nil
    end
  end

  defp normalize_humidity(0), do: 100
  defp normalize_humidity(val), do: val

  @spec parse_pressure(String.t()) :: float() | nil
  def parse_pressure(weather_data) do
    case Regex.run(~r/b(\d{5})/, weather_data) do
      [_, pressure] -> String.to_integer(pressure) / 10.0
      nil -> nil
    end
  end

  @spec parse_luminosity(String.t()) :: integer() | nil
  def parse_luminosity(weather_data) do
    case Regex.run(~r/[lL](\d{3})/, weather_data) do
      [_, luminosity] -> String.to_integer(luminosity)
      nil -> nil
    end
  end

  @spec parse_snow(String.t()) :: integer() | nil
  def parse_snow(weather_data) do
    case Regex.run(~r/s(\d{3})/, weather_data) do
      [_, snow] -> String.to_integer(snow)
      nil -> nil
    end
  end

  # Ambiguity and utility helpers
  @doc false
  def count_spaces(str) do
    str
    |> String.graphemes()
    |> Enum.count(fn c -> c == " " end)
  end

  @doc false
  def count_leading_braces(packet), do: count_leading_braces(packet, 0)

  @doc false
  def count_leading_braces(<<"}", rest::binary>>, count), do: count_leading_braces(rest, count + 1)

  @doc false
  def count_leading_braces(_packet, count), do: count

  @doc false
  def calculate_position_ambiguity(latitude, longitude) do
    lat_spaces = count_spaces(latitude)
    lon_spaces = count_spaces(longitude)

    Map.get(
      %{{0, 0} => 0, {1, 1} => 1, {2, 2} => 2, {3, 3} => 3, {4, 4} => 4},
      {lat_spaces, lon_spaces},
      0
    )
  end

  @doc false
  def calculate_compressed_ambiguity(compression_type) do
    case compression_type do
      " " -> 0
      "!" -> 1
      "\"" -> 2
      "#" -> 3
      "$" -> 4
      _ -> 0
    end
  end

  @doc false
  def find_matches(regex, text) do
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

  @doc false
  def parse_manufacturer(symbols) do
    Aprs.DeviceIdentification.identify_device(symbols)
  end

  @doc false
  defp apply_latitude_direction(lat_val, "S"), do: negate(lat_val)
  defp apply_latitude_direction(lat_val, _), do: lat_val

  @doc false
  defp apply_longitude_direction(lon_val, "W"), do: negate(lon_val)
  defp apply_longitude_direction(lon_val, _), do: lon_val

  @doc false
  def convert_to_base91(<<value::binary-size(4)>>) do
    [v1, v2, v3, v4] = to_charlist(value)
    (v1 - 33) * 91 * 91 * 91 + (v2 - 33) * 91 * 91 + (v3 - 33) * 91 + v4
  end

  @spec convert_compressed_cs(binary()) :: map()
  def convert_compressed_cs(cs) do
    [c, s] = to_charlist(cs)
    c = c - 33
    s = s - 33

    case c do
      x when x in ?!..?z ->
        %{
          course: s * 4,
          speed: Aprs.Convert.speed(1.08 ** s - 1, :knots, :mph)
        }

      ?Z ->
        %{
          range: 2 * 1.08 ** s
        }

      _ ->
        %{}
    end
  end

  @doc false
  def validate_position_data(latitude, longitude) do
    lat =
      case Regex.run(~r/^(\d{2})(\d{2}\.\d+)([NS])$/, latitude) do
        [_, degrees, minutes, direction] ->
          lat_val = add(new(degrees), Decimal.div(new(minutes), new("60")))
          apply_latitude_direction(lat_val, direction)

        _ ->
          nil
      end

    lon =
      case Regex.run(~r/^(\d{3})(\d{2}\.\d+)([EW])$/, longitude) do
        [_, degrees, minutes, direction] ->
          lon_val = add(new(degrees), Decimal.div(new(minutes), new("60")))
          apply_longitude_direction(lon_val, direction)

        _ ->
          nil
      end

    if is_struct(lat, Decimal) and is_struct(lon, Decimal) do
      {:ok, {lat, lon}}
    else
      {:error, :invalid_position}
    end
  end

  @doc false
  def validate_timestamp(_time), do: nil
end

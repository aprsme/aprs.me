defmodule Parser.Item do
  @moduledoc """
  APRS item parsing.
  """

  @doc """
  Parse an APRS item string. Returns a struct or error.
  """
  @spec parse(String.t()) :: map() | nil
  def parse(<<item_indicator, item_name_and_data::binary>>) when item_indicator in [?%, ?)] do
    case Regex.run(~r/^(.{1,9})([!_])(.*)$/, item_name_and_data) do
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

  def parse(data) do
    # Try to extract position from raw_data if present
    base = %{raw_data: data, data_type: :item}

    case Regex.run(~r/(\d{4,5}\.\d+[NS]).*([\/]?)(\d{5,6}\.\d+[EW])/, data) do
      [_, lat_str, _, lon_str] ->
        %{latitude: lat, longitude: lon} = Parser.Position.parse_aprs_position(lat_str, lon_str)
        Map.merge(base, %{latitude: lat, longitude: lon})

      _ ->
        base
    end
  end

  defp parse_item_position(position_data) do
    case position_data do
      <<latitude::binary-size(8), sym_table_id::binary-size(1), longitude::binary-size(9), symbol_code::binary-size(1),
        comment::binary>> ->
        %{latitude: lat, longitude: lon} =
          Parser.Position.parse_aprs_position(latitude, longitude)

        %{
          latitude: lat,
          longitude: lon,
          symbol_table_id: sym_table_id,
          symbol_code: symbol_code,
          comment: comment,
          position_format: :uncompressed
        }

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
end

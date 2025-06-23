# @dialyzer {:nowarn_function, parse: 1}
defmodule Parser.Object do
  @moduledoc """
  APRS object parsing.
  """

  @doc """
  Parse an APRS object string. Returns a struct or error.
  """
  @spec parse(String.t()) :: map() | nil
  def parse(<<";", object_name::binary-size(9), live_killed::binary-size(1), timestamp::binary-size(7), rest::binary>>) do
    position_data =
      case rest do
        <<"/", latitude_compressed::binary-size(4), longitude_compressed::binary-size(4), symbol_code::binary-size(1),
          cs::binary-size(2), compression_type::binary-size(1), comment::binary>> ->
          try do
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
          rescue
            _ -> %{latitude: nil, longitude: nil, comment: comment, position_format: :compressed}
          end

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

  def parse(data), do: %{data_type: :object, raw_data: data}
end

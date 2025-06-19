defmodule Parser.Position do
  @moduledoc """
  Uncompressed position parsing for APRS packets.
  """

  alias Parser.Types.Position

  @doc """
  Parse an uncompressed APRS position string. Returns a Position struct or nil.
  """
  @spec parse(String.t()) :: Position.t() | nil
  def parse(position_str) do
    # Example: "4903.50N/07201.75W>comment"
    case position_str do
      <<lat::binary-size(8), sym_table_id::binary-size(1), lon::binary-size(9), sym_code::binary-size(1),
        comment::binary>> ->
        %{latitude: lat_val, longitude: lon_val} = parse_aprs_position(lat, lon)
        ambiguity = calculate_position_ambiguity(lat, lon)
        dao_data = parse_dao_extension(comment)

        %Position{
          latitude: lat_val,
          longitude: lon_val,
          timestamp: nil,
          symbol_table_id: sym_table_id,
          symbol_code: sym_code,
          comment: comment,
          aprs_messaging?: false,
          compressed?: false,
          position_ambiguity: ambiguity,
          dao: dao_data
        }

      _ ->
        nil
    end
  end

  @doc false
  def parse_aprs_position(lat_str, lon_str) do
    # Parse latitude: DDMM.MM format
    lat =
      case Regex.run(
             ~r/^(
        \d{2})(\d{2}\.\d+)([NS])$/,
             lat_str
           ) do
        [_, degrees, minutes, direction] ->
          lat_val = String.to_integer(degrees) + String.to_float(minutes) / 60
          if direction == "S", do: -lat_val, else: lat_val

        _ ->
          nil
      end

    # Parse longitude: DDDMM.MM format
    lon =
      case Regex.run(~r/^(\d{3})(\d{2}\.\d+)([EW])$/, lon_str) do
        [_, degrees, minutes, direction] ->
          lon_val = String.to_integer(degrees) + String.to_float(minutes) / 60
          if direction == "W", do: -lon_val, else: lon_val

        _ ->
          nil
      end

    %{latitude: lat, longitude: lon}
  end

  @ambiguity_levels %{
    {0, 0} => 0,
    {1, 1} => 1,
    {2, 2} => 2,
    {3, 3} => 3,
    {4, 4} => 4
  }

  @doc false
  def calculate_position_ambiguity(latitude, longitude) do
    lat_spaces = count_spaces(latitude)
    lon_spaces = count_spaces(longitude)
    Map.get(@ambiguity_levels, {lat_spaces, lon_spaces}, 0)
  end

  @doc false
  def count_spaces(str) do
    str |> String.graphemes() |> Enum.count(&(&1 == " "))
  end

  @doc false
  def parse_dao_extension(comment) do
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

  # Move parse_aprs_position/2 and ambiguity/DAO helpers here from the main parser when refactoring further.
end

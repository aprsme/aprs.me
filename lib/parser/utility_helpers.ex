defmodule Parser.UtilityHelpers do
  @moduledoc """
  Utility and ambiguity helpers for APRS.
  """

  def count_spaces(str) do
    str |> String.graphemes() |> Enum.count(fn c -> c == " " end)
  end

  def count_leading_braces(packet), do: count_leading_braces(packet, 0)

  def count_leading_braces(<<"}", rest::binary>>, count), do: count_leading_braces(rest, count + 1)

  def count_leading_braces(_packet, count), do: count

  def calculate_position_ambiguity(latitude, longitude) do
    lat_spaces = count_spaces(latitude)
    lon_spaces = count_spaces(longitude)

    Map.get(
      %{{0, 0} => 0, {1, 1} => 1, {2, 2} => 2, {3, 3} => 3, {4, 4} => 4},
      {lat_spaces, lon_spaces},
      0
    )
  end

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

  def validate_position_data(latitude, longitude) do
    import Decimal, only: [new: 1, add: 2, negate: 1]

    lat =
      case Regex.run(~r/^(\d{2})(\d{2}\.\d+)([NS])$/, latitude) do
        [_, degrees, minutes, direction] ->
          lat_val = add(new(degrees), Decimal.div(new(minutes), new("60")))
          if direction == "S", do: negate(lat_val), else: lat_val

        _ ->
          nil
      end

    lon =
      case Regex.run(~r/^(\d{3})(\d{2}\.\d+)([EW])$/, longitude) do
        [_, degrees, minutes, direction] ->
          lon_val = add(new(degrees), Decimal.div(new(minutes), new("60")))
          if direction == "W", do: negate(lon_val), else: lon_val

        _ ->
          nil
      end

    if is_struct(lat, Decimal) and is_struct(lon, Decimal) do
      {:ok, {lat, lon}}
    else
      {:error, :invalid_position}
    end
  end

  def validate_timestamp(_time), do: nil
end

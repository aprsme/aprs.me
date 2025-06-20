defmodule Parser.Types do
  @moduledoc """
  Core types and structs for APRS parser.
  """

  alias Parser.Types.MicE

  @type mice :: MicE.t()

  defmodule Packet do
    @moduledoc false
    @type t :: %__MODULE__{}
    defstruct [
      :id,
      :sender,
      :path,
      :destination,
      :information_field,
      :data_type,
      :base_callsign,
      :ssid,
      :data_extended,
      :received_at
    ]
  end

  defmodule Position do
    @moduledoc false
    @type t :: %__MODULE__{}
    defstruct [
      :latitude,
      :longitude,
      :timestamp,
      :symbol_table_id,
      :symbol_code,
      :comment,
      :aprs_messaging?,
      :compressed?,
      :position_ambiguity,
      :dao
    ]

    @doc """
    Parse APRS lat/lon strings (e.g., "3339.13N", "11759.13W") into a map with latitude and longitude.
    """
    def from_aprs(lat_str, lon_str) do
      import Decimal, only: [new: 1, add: 2, negate: 1]

      lat =
        case Regex.run(~r/^(\d{2})(\d{2}\.\d+)([NS])$/, lat_str) do
          [_, degrees, minutes, direction] ->
            lat_val = add(new(degrees), Decimal.div(new(minutes), new("60")))
            if direction == "S", do: negate(lat_val), else: lat_val

          _ ->
            nil
        end

      lon =
        case Regex.run(~r/^(\d{3})(\d{2}\.\d+)([EW])$/, lon_str) do
          [_, degrees, minutes, direction] ->
            lon_val = add(new(degrees), Decimal.div(new(minutes), new("60")))
            if direction == "W", do: negate(lon_val), else: lon_val

          _ ->
            nil
        end

      %{latitude: lat, longitude: lon}
    end

    @doc """
    Return a map with latitude and longitude from decimal values.
    """
    def from_decimal(lat, lon) do
      %{latitude: Decimal.new(to_string(lat)), longitude: Decimal.new(to_string(lon))}
    end
  end

  defmodule ParseError do
    @moduledoc false
    @type t :: %__MODULE__{}
    defstruct [
      :error_code,
      :error_message,
      :raw_data
    ]
  end

  # Add more structs as needed for NMEA, PHG, etc.
end

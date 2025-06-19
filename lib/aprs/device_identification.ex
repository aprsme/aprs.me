defmodule Aprs.DeviceIdentification do
  @moduledoc """
  Handles APRS device identification based on the APRS device identification database.
  """

  @device_patterns [
    {~r/^ \x00\x00$/, "Original MIC-E"},
    {~r/^>\x00\^$/, "Kenwood TH-D74"},
    {~r/^>\x00\x00$/, "Kenwood TH-D74A"},
    {~r/^]\x00=$/, "Kenwood DM-710"},
    {~r/^]\x00\x00$/, "Kenwood DM-700"},
    {~r/^`_ $/, "Yaesu VX-8"},
    {~r/^`_\"$/, "Yaesu FTM-350"},
    {~r/^`_#$/, "Yaesu VX-8G"},
    {~r/^`_\$$/, "Yaesu FT1D"},
    {~r/^`_%$/, "Yaesu FTM-400DR"},
    {~r/^`_\)$/, "Yaesu FTM-100D"},
    {~r/^`_\($/, "Yaesu FT2D"},
    {~r/^` X$/, "AP510"},
    {~r/^`\x00\x00$/, "Mic-Emsg"},
    {~r/^'\|3$/, "Byonics TinyTrack3"},
    {~r/^'\|4$/, "Byonics TinyTrack4"},
    {~r/^':4$/, "SCS GmbH & Co. P4dragon DR-7400 modems"},
    {~r/^':8$/, "SCS GmbH & Co. P4dragon DR-7800 modems"},
    {~r/^'\x00\x00$/, "McTrackr"},
    {~r/^\x00\"\x00$/, "Hamhud"},
    {~r/^\x00\/\x00$/, "Argent"},
    {~r/^\x00\^\x00$/, "HinzTec anyfrog"},
    {~r/^\x00\*\x00$/, "APOZxx www.KissOZ.dk Tracker. OZ1EKD and OZ7HVO"},
    {~r/^\x00~\x00$/, "Other"}
  ]

  @doc """
  Identifies the manufacturer and model of an APRS device based on its symbol pattern.
  Returns a tuple of {manufacturer, model} or "Unknown" if the device cannot be identified.
  """
  @spec identify_device(String.t()) :: String.t()
  def identify_device(symbols) do
    Enum.find_value(@device_patterns, "Unknown", fn {regex, name} ->
      if Regex.match?(regex, symbols), do: name
    end)
  end

  @doc """
  Returns a list of all known device manufacturers.
  """
  @spec known_manufacturers() :: [String.t()]
  def known_manufacturers do
    [
      "Original MIC-E",
      "Kenwood",
      "Yaesu",
      "AP510",
      "Byonics",
      "SCS GmbH & Co.",
      "McTrackr",
      "Hamhud",
      "Argent",
      "HinzTec",
      "APOZxx",
      "Other"
    ]
  end

  @doc """
  Returns a list of all known device models for a given manufacturer.
  """
  @spec known_models(String.t()) :: [String.t()]
  def known_models(manufacturer) do
    case manufacturer do
      "Kenwood" -> ["TH-D74", "TH-D74A", "DM-710", "DM-700"]
      "Yaesu" -> ["VX-8", "FTM-350", "VX-8G", "FT1D", "FTM-400DR", "FTM-100D", "FT2D"]
      "Byonics" -> ["TinyTrack3", "TinyTrack4"]
      "SCS GmbH & Co." -> ["P4dragon DR-7400 modems", "P4dragon DR-7800 modems"]
      _ -> []
    end
  end
end

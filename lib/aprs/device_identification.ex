defmodule Aprs.DeviceIdentification do
  @moduledoc """
  Handles APRS device identification based on the APRS device identification database.
  """

  @doc """
  Identifies the manufacturer and model of an APRS device based on its symbol pattern.
  Returns a tuple of {manufacturer, model} or "Unknown" if the device cannot be identified.
  """
  @spec identify_device(String.t()) :: String.t()
  def identify_device(symbols) do
    case symbols do
      # Original MIC-E devices
      " " <> <<0, 0>> -> "Original MIC-E"
      # Kenwood devices
      ">" <> <<0>> <> "^" -> "Kenwood TH-D74"
      ">" <> <<0, 0>> -> "Kenwood TH-D74A"
      "]" <> <<0>> <> "=" -> "Kenwood DM-710"
      "]" <> <<0, 0>> -> "Kenwood DM-700"
      # Yaesu devices
      "`_" <> " " -> "Yaesu VX-8"
      "`_" <> "\"" -> "Yaesu FTM-350"
      "`_" <> "#" -> "Yaesu VX-8G"
      "`_" <> "$" -> "Yaesu FT1D"
      "`_" <> "%" -> "Yaesu FTM-400DR"
      "`_" <> ")" -> "Yaesu FTM-100D"
      "`_" <> "(" -> "Yaesu FT2D"
      # Other devices
      "` X" -> "AP510"
      "`" <> <<0, 0>> -> "Mic-Emsg"
      "'|3" -> "Byonics TinyTrack3"
      "'|4" -> "Byonics TinyTrack4"
      "':4" -> "SCS GmbH & Co. P4dragon DR-7400 modems"
      "':8" -> "SCS GmbH & Co. P4dragon DR-7800 modems"
      "'" <> <<0, 0>> -> "McTrackr"
      <<0>> <> "\"" <> <<0>> -> "Hamhud"
      <<0>> <> "/" <> <<0>> -> "Argent"
      <<0>> <> "^" <> <<0>> -> "HinzTec anyfrog"
      <<0>> <> "*" <> <<0>> -> "APOZxx www.KissOZ.dk Tracker. OZ1EKD and OZ7HVO"
      <<0>> <> "~" <> <<0>> -> "Other"
      # Unknown devices
      _ -> "Unknown"
    end
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

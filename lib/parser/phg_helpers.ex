defmodule Parser.PHGHelpers do
  @moduledoc """
  PHG/DF helper functions for APRS.
  """

  @type phg_power :: {integer() | nil, String.t()}
  @type phg_height :: {integer() | nil, String.t()}
  @type phg_gain :: {integer() | nil, String.t()}
  @type phg_directivity :: {integer() | nil, String.t()}
  @type df_strength :: {integer() | nil, String.t()}

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
end

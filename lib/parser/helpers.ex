defmodule Parser.Helpers do
  @moduledoc """
  This module has been split into domain-specific helpers:
  - Parser.NMEAHelpers
  - Parser.PHGHelpers
  - Parser.CompressedPositionHelpers
  - Parser.KISSHelpers
  - Parser.TelemetryHelpers
  - Parser.WeatherHelpers
  - Parser.UtilityHelpers
  Update your code to use the new modules directly.
  """
  @compile {:no_warn_undefined, __MODULE__}
end

defmodule Parser.Telemetry do
  @moduledoc """
  APRS telemetry parsing.
  """

  @doc """
  Parse an APRS telemetry string. Returns a struct or error.
  """
  @spec parse(String.t()) :: map() | nil
  def parse("T#" <> rest) do
    case String.split(rest, ",") do
      [seq | [_ | _] = values] ->
        analog_values = Enum.take(values, 5)
        digital_values = values |> Enum.drop(5) |> Enum.take(8)

        %{
          sequence_number: Parser.Helpers.parse_telemetry_sequence(seq),
          analog_values: Parser.Helpers.parse_analog_values(analog_values),
          digital_values: Parser.Helpers.parse_digital_values(digital_values),
          data_type: :telemetry,
          raw_data: rest
        }

      _ ->
        %{
          raw_data: rest,
          data_type: :telemetry
        }
    end
  end

  def parse(<<":PARM.", rest::binary>>) do
    %{
      data_type: :telemetry_parameters,
      parameter_names: String.split(rest, ",", trim: true),
      raw_data: rest
    }
  end

  def parse(<<":EQNS.", rest::binary>>) do
    equations =
      rest
      |> String.split(",", trim: true)
      |> Enum.chunk_every(3)
      |> Enum.map(fn [a, b, c] ->
        %{
          a: Parser.Helpers.parse_coefficient(a),
          b: Parser.Helpers.parse_coefficient(b),
          c: Parser.Helpers.parse_coefficient(c)
        }
      end)

    %{
      data_type: :telemetry_equations,
      equations: equations,
      raw_data: rest
    }
  end

  def parse(<<":UNIT.", rest::binary>>) do
    %{
      data_type: :telemetry_units,
      units: String.split(rest, ",", trim: true),
      raw_data: rest
    }
  end

  def parse(<<":BITS.", rest::binary>>) do
    case String.split(rest, ",", trim: true) do
      [bits_sense | project_names] ->
        %{
          data_type: :telemetry_bits,
          bits_sense: String.to_charlist(bits_sense),
          project_names: project_names,
          raw_data: rest
        }

      [] ->
        %{
          data_type: :telemetry_bits,
          bits_sense: [],
          project_names: [],
          raw_data: rest
        }
    end
  end

  def parse(data), do: %{raw_data: data, data_type: :telemetry}
end

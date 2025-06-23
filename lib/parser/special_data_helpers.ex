defmodule Parser.SpecialDataHelpers do
  @moduledoc """
  Helpers for special APRS data types: PEET logging and invalid/test data.
  """

  def parse_peet_logging(<<"*", peet_data::binary>>) do
    %{
      peet_data: peet_data,
      data_type: :peet_logging
    }
  end

  def parse_peet_logging(data) do
    %{
      peet_data: data,
      data_type: :peet_logging
    }
  end

  def parse_invalid_test_data(<<",", test_data::binary>>) do
    %{
      test_data: test_data,
      data_type: :invalid_test_data
    }
  end

  def parse_invalid_test_data(data) do
    %{
      test_data: data,
      data_type: :invalid_test_data
    }
  end
end

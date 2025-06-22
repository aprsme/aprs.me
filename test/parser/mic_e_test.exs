defmodule Parser.MicETest do
  use ExUnit.Case, async: true

  alias Parser.MicE

  describe "parse/2" do
    test "returns parsed map for valid Mic-E destination and data" do
      # Example valid destination and data (values are illustrative)
      destination = "ABCD12"
      data = <<40, 41, 42, 43, 44, 45, 46, 47>> <> "rest"
      result = MicE.parse(data, destination)
      assert is_map(result)
      assert result[:data_type] == :mic_e or result[:data_type] == :mic_e_error
    end

    test "returns error map for invalid destination length" do
      destination = "SHORT"
      data = <<40, 41, 42, 43, 44, 45, 46, 47>>
      result = MicE.parse(data, destination)
      assert result[:data_type] == :mic_e_error
      assert result[:latitude] == nil
      assert result[:longitude] == nil
    end

    test "returns error map for invalid information field length" do
      destination = "ABCDEF"
      data = <<1, 2, 3>>
      result = MicE.parse(data, destination)
      assert result[:data_type] == :mic_e_error
      assert result[:latitude] == nil
      assert result[:longitude] == nil
    end

    test "returns error map for invalid characters in destination" do
      destination = "!!!!!!"
      data = <<40, 41, 42, 43, 44, 45, 46, 47>>
      result = MicE.parse(data, destination)
      assert result[:data_type] == :mic_e_error
    end

    test "returns error map for nil destination" do
      data = <<40, 41, 42, 43, 44, 45, 46, 47>>
      result = MicE.parse(data, nil)
      assert result[:data_type] == :mic_e_error
    end
  end
end

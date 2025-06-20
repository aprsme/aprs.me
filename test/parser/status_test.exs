defmodule Parser.StatusTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Parser.Status

  describe "parse/1" do
    test "returns a status map for valid input" do
      assert Status.parse(">Test status message") == %{
               data_type: :status,
               status_text: "Test status message"
             }
    end

    property "always returns a map with :data_type == :status for any string" do
      check all s <- StreamData.string(:ascii, min_length: 1, max_length: 30) do
        result = Status.parse(s)
        assert is_map(result)
        assert result[:data_type] == :status
      end
    end
  end
end

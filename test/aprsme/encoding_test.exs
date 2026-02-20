defmodule Aprsme.EncodingTest do
  use ExUnit.Case

  alias Aprsme.Encoding

  describe "sanitize_string/1 fast-path for clean strings" do
    test "returns clean ASCII string unchanged" do
      input = "VALID1>APRS,WIDE1-1:Test packet"
      assert Encoding.sanitize_string(input) == input
    end

    test "returns clean ASCII string with allowed whitespace unchanged" do
      input = "Hello\tWorld\nNew line\r\nCRLF"
      result = Encoding.sanitize_string(input)
      assert result == input
    end

    test "still cleans strings with control characters" do
      # String with null byte (control character that should be removed)
      input = "Hello\x00World"
      result = Encoding.sanitize_string(input)
      assert result == "HelloWorld"
    end

    test "still cleans strings with DEL character" do
      input = "Hello\x7FWorld"
      result = Encoding.sanitize_string(input)
      assert result == "HelloWorld"
    end

    test "handles empty string" do
      assert Encoding.sanitize_string("") == ""
    end

    test "handles non-binary input" do
      assert Encoding.sanitize_string(nil) == ""
      assert Encoding.sanitize_string(123) == ""
    end

    test "handles valid UTF-8 multi-byte characters" do
      input = "Café résumé"
      result = Encoding.sanitize_string(input)
      assert String.valid?(result)
      assert String.contains?(result, "Caf")
    end

    test "handles latin1 encoded binary" do
      # Latin1 bytes for accented characters (>= 160, valid Latin1 printable)
      invalid_binary = <<85, 78, 73, 211, 78, 32, 80, 65, 78, 65, 77, 69, 209, 65>>
      result = Encoding.sanitize_string(invalid_binary)
      assert String.valid?(result)
      # 211 = Ó, 209 = Ñ — both above 159, so they convert to real characters
      assert result == "UNIÓN PANAMEÑA"
    end
  end

  describe "sanitize_string/1 C1 control range (128-159)" do
    test "latin1 bytes 128-159 become replacement characters instead of being silently deleted" do
      input = <<72, 101, 108, 108, 111, 130>>
      result = Encoding.sanitize_string(input)
      assert String.valid?(result)
      assert String.contains?(result, "\uFFFD")
    end

    test "latin1 byte 128 becomes replacement character" do
      input = <<65, 128, 66>>
      result = Encoding.sanitize_string(input)
      assert String.valid?(result)
      assert result == "A\uFFFDB"
    end

    test "latin1 byte 159 becomes replacement character" do
      input = <<65, 159, 66>>
      result = Encoding.sanitize_string(input)
      assert String.valid?(result)
      assert result == "A\uFFFDB"
    end

    test "latin1 bytes above 159 still convert to proper characters" do
      input = <<67, 97, 102, 233>>
      result = Encoding.sanitize_string(input)
      assert String.valid?(result)
      assert result == "Café"
    end
  end

  describe "to_float_safe/1" do
    test "parses valid numeric string" do
      assert Encoding.to_float_safe("3.14") == {:ok, 3.14}
      assert Encoding.to_float_safe("42") == {:ok, 42.0}
    end

    test "trims whitespace" do
      assert Encoding.to_float_safe("  3.14  ") == {:ok, 3.14}
    end

    test "truncates strings longer than 30 characters" do
      # "1.0" padded with trailing zeros — valid after truncation
      long = "1." <> String.duplicate("0", 40)
      assert Encoding.to_float_safe(long) == {:ok, 1.0}
    end

    test "returns nil for out-of-range float" do
      assert is_nil(Encoding.to_float_safe("9.1e15"))
      assert is_nil(Encoding.to_float_safe("-9.1e15"))
    end

    test "returns value for in-range float" do
      assert {:ok, f} = Encoding.to_float_safe("1000000.0")
      assert f == 1_000_000.0
    end

    test "returns nil for invalid string" do
      assert is_nil(Encoding.to_float_safe("abc"))
      assert is_nil(Encoding.to_float_safe(""))
    end

    test "returns nil for non-binary input" do
      assert is_nil(Encoding.to_float_safe(nil))
      assert is_nil(Encoding.to_float_safe(123))
      assert is_nil(Encoding.to_float_safe(:atom))
    end

    test "handles scientific notation" do
      assert {:ok, f} = Encoding.to_float_safe("1.5e3")
      assert f == 1500.0
    end

    test "handles zero" do
      assert Encoding.to_float_safe("0") == {:ok, 0.0}
      assert Encoding.to_float_safe("0.0") == {:ok, 0.0}
    end
  end

  describe "has_weather_data/4" do
    test "returns true when single field present" do
      assert Encoding.has_weather_data(72.0, nil, nil, nil)
      assert Encoding.has_weather_data(nil, 50, nil, nil)
      assert Encoding.has_weather_data(nil, nil, 5.0, nil)
      assert Encoding.has_weather_data(nil, nil, nil, 1013.25)
    end

    test "returns true when multiple fields present" do
      assert Encoding.has_weather_data(72.0, 50, 5.0, 1013.25)
    end

    test "returns false when all nil" do
      refute Encoding.has_weather_data(nil, nil, nil, nil)
    end

    test "treats zero as valid weather data" do
      assert Encoding.has_weather_data(0, nil, nil, nil)
      assert Encoding.has_weather_data(nil, 0, nil, nil)
    end

    test "treats negative values as valid weather data" do
      assert Encoding.has_weather_data(-10, nil, nil, nil)
    end
  end

  describe "to_hex/1" do
    test "returns empty string for non-binary" do
      assert Encoding.to_hex(nil) == ""
      assert Encoding.to_hex(123) == ""
    end

    test "handles boundary byte values" do
      assert Encoding.to_hex(<<0>>) == "00"
      assert Encoding.to_hex(<<255>>) == "FF"
    end
  end
end

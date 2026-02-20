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
      # Latin1 bytes for accented characters
      invalid_binary = <<85, 78, 73, 211, 78, 32, 80, 65, 78, 65, 77, 69, 209, 65>>
      result = Encoding.sanitize_string(invalid_binary)
      assert String.valid?(result)
    end
  end
end

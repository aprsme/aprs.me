defmodule Aprsme.CallsignTest do
  use ExUnit.Case, async: true

  alias Aprsme.Callsign

  describe "valid?/1" do
    test "validates standard callsigns" do
      assert Callsign.valid?("K5ABC")
      assert Callsign.valid?("W1XYZ")
      assert Callsign.valid?("N0CALL")
    end

    test "validates callsigns with SSID" do
      assert Callsign.valid?("K5ABC-1")
      assert Callsign.valid?("W1XYZ-15")
      assert Callsign.valid?("N0CALL-9")
    end

    test "validates callsigns with hyphens in base callsign like VE-KTKI" do
      assert Callsign.valid?("VE-KTKI")
      assert Callsign.valid?("VE-KTKI-1")
      assert Callsign.valid?("VE-TEST")
    end

    test "rejects only empty callsigns" do
      refute Callsign.valid?("")
      refute Callsign.valid?(" ")

      # Now accepts any non-empty string
      assert Callsign.valid?("123")
      assert Callsign.valid?("-ABC")
      assert Callsign.valid?("ABC-")
      assert Callsign.valid?("ABC-123")
      assert Callsign.valid?("ABC--1")
    end

    test "handles nil input" do
      refute Callsign.valid?(nil)
    end
  end

  describe "normalize/1" do
    test "converts to uppercase and trims whitespace" do
      assert Callsign.normalize("k5abc") == "K5ABC"
      assert Callsign.normalize(" W1XYZ ") == "W1XYZ"
      assert Callsign.normalize("  n0call-9  ") == "N0CALL-9"
    end

    test "normalizes callsigns with hyphens in base" do
      assert Callsign.normalize("ve-ktki") == "VE-KTKI"
      assert Callsign.normalize(" VE-KTKI-1 ") == "VE-KTKI-1"
    end

    test "handles nil input" do
      assert Callsign.normalize(nil) == ""
    end
  end
end

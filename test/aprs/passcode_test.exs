defmodule Aprs.PasscodeTest do
  use ExUnit.Case
  doctest Aprs.Passcode

  describe "generate/1" do
    test "generates correct passcode for W5ISP" do
      assert Aprs.Passcode.generate("W5ISP") == 15748
    end

    test "handles callsigns with SSIDs" do
      assert Aprs.Passcode.generate("W5ISP-1") == 15748
    end

    test "handles lowercase callsigns" do
      assert Aprs.Passcode.generate("w5isp") == 15748
    end

    test "handles long callsigns" do
      assert Aprs.Passcode.generate("W5ISP-LONG") == 15748
    end
  end
end

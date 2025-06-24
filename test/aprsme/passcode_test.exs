defmodule Aprsme.PasscodeTest do
  use ExUnit.Case

  doctest Aprsme.Passcode

  describe "generate/1" do
    test "generates correct passcode for W5ISP" do
      assert Aprsme.Passcode.generate("W5ISP") == 15_748
    end

    test "handles callsigns with SSIDs" do
      assert Aprsme.Passcode.generate("W5ISP-1") == 15_748
    end

    test "handles lowercase callsigns" do
      assert Aprsme.Passcode.generate("w5isp") == 15_748
    end

    test "handles long callsigns" do
      assert Aprsme.Passcode.generate("W5ISP-LONG") == 15_748
    end
  end
end

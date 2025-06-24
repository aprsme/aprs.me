defmodule Aprsme.ConvertTest do
  use ExUnit.Case

  alias Aprsme.Convert

  describe "wind/3" do
    test "converts ultimeter mph" do
      assert Convert.wind(55, :ultimeter, :mph) == 3.417541556
    end
  end

  describe "temp/3" do
    test "converts ultimeter f" do
      assert Convert.temp(55, :ultimeter, :f) == 5.5
    end
  end

  describe "speed/3" do
    test "converts knots to mph" do
      assert Convert.speed(55, :knots, :mph) == 63.29
    end
  end
end

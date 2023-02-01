defmodule Aprs.ConvertTest do
  use ExUnit.Case
  alias Aprs.Convert

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
end

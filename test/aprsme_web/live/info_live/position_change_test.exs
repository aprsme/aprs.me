defmodule AprsmeWeb.InfoLive.PositionChangeTest do
  @moduledoc """
  Tests for position change detection in InfoLive
  """
  use ExUnit.Case, async: true

  alias AprsmeWeb.InfoLive.Show

  describe "position_changed?/2" do
    test "returns true when current packet is nil" do
      new_packet = %{lat: 40.7128, lon: -74.0060}
      assert Show.position_changed_for_test(nil, new_packet)
    end

    test "returns false when new packet is nil" do
      current_packet = %{lat: 40.7128, lon: -74.0060}
      refute Show.position_changed_for_test(current_packet, nil)
    end

    test "returns true when position changed significantly" do
      current_packet = %{lat: 40.7128, lon: -74.0060}
      # ~0.8km difference
      new_packet = %{lat: 40.7200, lon: -74.0060}
      assert Show.position_changed_for_test(current_packet, new_packet)
    end

    test "returns false when position changed minimally" do
      current_packet = %{lat: 40.7128, lon: -74.0060}
      # ~10m difference
      new_packet = %{lat: 40.7129, lon: -74.0061}
      refute Show.position_changed_for_test(current_packet, new_packet)
    end

    test "returns true when longitude changed significantly" do
      current_packet = %{lat: 40.7128, lon: -74.0060}
      # longitude changed
      new_packet = %{lat: 40.7128, lon: -74.0200}
      assert Show.position_changed_for_test(current_packet, new_packet)
    end

    test "handles Decimal values" do
      current_packet = %{lat: Decimal.new("40.7128"), lon: Decimal.new("-74.0060")}
      new_packet = %{lat: Decimal.new("40.7200"), lon: Decimal.new("-74.0060")}
      assert Show.position_changed_for_test(current_packet, new_packet)
    end
  end
end

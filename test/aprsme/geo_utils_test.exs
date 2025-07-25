defmodule Aprsme.GeoUtilsTest do
  use ExUnit.Case, async: true

  alias Aprsme.GeoUtils

  describe "haversine_distance/4" do
    test "calculates zero distance for same point" do
      assert GeoUtils.haversine_distance(33.16961, -96.4921, 33.16961, -96.4921) == 0.0
    end

    test "calculates small distances accurately" do
      # About 1.1 meters north
      distance = GeoUtils.haversine_distance(33.16961, -96.4921, 33.16962, -96.4921)
      assert_in_delta distance, 1.11, 0.1
    end

    test "calculates GPS drift-like distances" do
      # Typical GPS drift of ~5 meters
      distance = GeoUtils.haversine_distance(33.16961, -96.4921, 33.169655, -96.4921)
      assert_in_delta distance, 5.0, 0.5
    end

    test "calculates larger distances" do
      # About 100 meters
      distance = GeoUtils.haversine_distance(33.16961, -96.4921, 33.17061, -96.4921)
      assert_in_delta distance, 111.2, 1.0
    end

    test "handles nil inputs" do
      assert GeoUtils.haversine_distance(nil, -96.4921, 33.16962, -96.4921) == nil
      assert GeoUtils.haversine_distance(33.16961, nil, 33.16962, -96.4921) == nil
      assert GeoUtils.haversine_distance(33.16961, -96.4921, nil, -96.4921) == nil
      assert GeoUtils.haversine_distance(33.16961, -96.4921, 33.16962, nil) == nil
    end

    test "handles non-numeric inputs" do
      assert GeoUtils.haversine_distance("33.16961", -96.4921, 33.16962, -96.4921) == nil
      assert GeoUtils.haversine_distance(33.16961, "invalid", 33.16962, -96.4921) == nil
    end
  end

  describe "significant_movement?/5" do
    test "returns false for no movement" do
      refute GeoUtils.significant_movement?(33.16961, -96.4921, 33.16961, -96.4921)
    end

    test "returns false for GPS drift under default threshold" do
      # About 5 meters - typical GPS drift
      refute GeoUtils.significant_movement?(33.16961, -96.4921, 33.169655, -96.4921)
    end

    test "returns true for movement over default threshold" do
      # About 55 meters movement (over 50m default threshold)
      assert GeoUtils.significant_movement?(33.16961, -96.4921, 33.1701, -96.4921)
    end

    test "respects custom threshold" do
      # About 5 meters movement
      lat1 = 33.16961
      lon1 = -96.4921
      lat2 = 33.169655
      lon2 = -96.4921

      # With 3 meter threshold, should be significant
      assert GeoUtils.significant_movement?(lat1, lon1, lat2, lon2, 3)

      # With 10 meter threshold (default), should not be significant
      refute GeoUtils.significant_movement?(lat1, lon1, lat2, lon2, 10)

      # With 20 meter threshold, definitely not significant
      refute GeoUtils.significant_movement?(lat1, lon1, lat2, lon2, 20)
    end

    test "handles nil inputs" do
      refute GeoUtils.significant_movement?(nil, -96.4921, 33.16962, -96.4921)
      refute GeoUtils.significant_movement?(33.16961, nil, 33.16962, -96.4921)
      refute GeoUtils.significant_movement?(33.16961, -96.4921, nil, -96.4921)
      refute GeoUtils.significant_movement?(33.16961, -96.4921, 33.16962, nil)
    end
  end
end

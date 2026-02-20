defmodule Aprsme.Packets.QueryBuilderTest do
  use Aprsme.DataCase

  import Ecto.Query

  alias Aprsme.Packet
  alias Aprsme.Packets.QueryBuilder
  alias Aprsme.PacketsFixtures

  describe "within_bounds/2" do
    test "filters packets within normal bounds" do
      inside =
        PacketsFixtures.packet_fixture(%{
          sender: "TEST-1",
          lat: Decimal.new("40.5"),
          lon: Decimal.new("-73.5")
        })

      _outside =
        PacketsFixtures.packet_fixture(%{
          sender: "TEST-2",
          lat: Decimal.new("50.0"),
          lon: Decimal.new("-73.5")
        })

      # Test normal bounds [west, south, east, north]
      query = QueryBuilder.within_bounds(Packet, [-74.0, 40.0, -73.0, 41.0])
      results = Aprsme.Repo.all(query)

      assert length(results) == 1
      assert hd(results).id == inside.id
    end

    test "handles antimeridian crossing bounds" do
      west_side =
        PacketsFixtures.packet_fixture(%{
          sender: "WEST-1",
          lat: Decimal.new("0.0"),
          lon: Decimal.new("175.0")
        })

      east_side =
        PacketsFixtures.packet_fixture(%{
          sender: "EAST-1",
          lat: Decimal.new("0.0"),
          lon: Decimal.new("-175.0")
        })

      _middle =
        PacketsFixtures.packet_fixture(%{
          sender: "MIDDLE-1",
          lat: Decimal.new("0.0"),
          lon: Decimal.new("0.0")
        })

      # Test antimeridian crossing bounds [west=170, south=-10, east=-170, north=10]
      query = QueryBuilder.within_bounds(Packet, [170.0, -10.0, -170.0, 10.0])
      results = Aprsme.Repo.all(query)

      assert length(results) == 2
      result_ids = results |> Enum.map(& &1.id) |> Enum.sort()
      expected_ids = Enum.sort([west_side.id, east_side.id])
      assert result_ids == expected_ids
    end

    test "returns query unchanged with invalid bounds" do
      original_query = from(p in Packet)

      # Test with nil
      assert QueryBuilder.within_bounds(original_query, nil) == original_query

      # Test with wrong number of elements
      assert QueryBuilder.within_bounds(original_query, [1, 2, 3]) == original_query

      # Test with non-numeric values
      assert QueryBuilder.within_bounds(original_query, ["a", "b", "c", "d"]) == original_query
    end
  end

  describe "weather_only/1" do
    test "filters to weather packets using has_weather column" do
      _weather =
        PacketsFixtures.packet_fixture(%{
          sender: "WX-QBT",
          temperature: 72.0,
          has_weather: true
        })

      _non_weather =
        PacketsFixtures.packet_fixture(%{
          sender: "NOWX-QBT",
          comment: "No weather"
        })

      query = QueryBuilder.weather_only(Packet)
      results = Aprsme.Repo.all(query)

      assert Enum.all?(results, fn p -> p.has_weather == true end)
    end
  end
end

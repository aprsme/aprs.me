defmodule Aprsme.Packets.QueryBuilderTest do
  use Aprsme.DataCase

  import Ecto.Query

  alias Aprsme.Packet
  alias Aprsme.Packets.QueryBuilder

  describe "within_bounds/2" do
    test "filters packets within normal bounds" do
      # Create test packets
      {:ok, inside} =
        Aprsme.Repo.insert(%Packet{
          sender: "TEST-1",
          lat: 40.5,
          lon: -73.5,
          has_position: true,
          received_at: DateTime.truncate(DateTime.utc_now(), :second)
        })

      {:ok, _outside} =
        Aprsme.Repo.insert(%Packet{
          sender: "TEST-2",
          lat: 50.0,
          lon: -73.5,
          has_position: true,
          received_at: DateTime.truncate(DateTime.utc_now(), :second)
        })

      # Test normal bounds [west, south, east, north]
      query = QueryBuilder.within_bounds(Packet, [-74.0, 40.0, -73.0, 41.0])
      results = Aprsme.Repo.all(query)

      assert length(results) == 1
      assert hd(results).id == inside.id
    end

    test "handles antimeridian crossing bounds" do
      # Create test packets
      {:ok, west_side} =
        Aprsme.Repo.insert(%Packet{
          sender: "WEST-1",
          lat: 0.0,
          lon: 175.0,
          has_position: true,
          received_at: DateTime.truncate(DateTime.utc_now(), :second)
        })

      {:ok, east_side} =
        Aprsme.Repo.insert(%Packet{
          sender: "EAST-1",
          lat: 0.0,
          lon: -175.0,
          has_position: true,
          received_at: DateTime.truncate(DateTime.utc_now(), :second)
        })

      {:ok, _middle} =
        Aprsme.Repo.insert(%Packet{
          sender: "MIDDLE-1",
          lat: 0.0,
          lon: 0.0,
          has_position: true,
          received_at: DateTime.truncate(DateTime.utc_now(), :second)
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
end

defmodule Aprsme.Packets.ClusteringTest do
  use Aprsme.DataCase

  alias Aprsme.Packet
  alias Aprsme.Packets.Clustering

  describe "cluster_packets/3" do
    test "returns raw packets when zoom level is greater than 8" do
      packets = [
        %Packet{id: 1, lat: Decimal.new("40.7128"), lon: Decimal.new("-74.0060")},
        %Packet{id: 2, lat: Decimal.new("40.7580"), lon: Decimal.new("-73.9855")}
      ]

      result = Clustering.cluster_packets(packets, 9, %{})
      assert result == {:raw_packets, packets}
    end

    test "handles packets with string keys" do
      packets = [
        %{"id" => "1", "lat" => 40.7128, "lon" => -74.0060},
        %{"id" => "2", "lat" => "40.7580", "lon" => "-73.9855"}
      ]

      result = Clustering.cluster_packets(packets, 7, %{})
      assert {:heat_map, clusters} = result
      assert is_list(clusters)
      assert length(clusters) > 0
    end

    test "returns clustered data when zoom level is 8 or less" do
      packets = [
        %Packet{id: 1, lat: Decimal.new("40.7128"), lon: Decimal.new("-74.0060")},
        %Packet{id: 2, lat: Decimal.new("40.7580"), lon: Decimal.new("-73.9855")},
        %Packet{id: 3, lat: Decimal.new("51.5074"), lon: Decimal.new("-0.1278")}
      ]

      result = Clustering.cluster_packets(packets, 7, %{})
      assert {:heat_map, clusters} = result
      assert is_list(clusters)
      assert length(clusters) > 0

      # Check cluster structure
      [first_cluster | _] = clusters
      assert Map.has_key?(first_cluster, :lat)
      assert Map.has_key?(first_cluster, :lng)
      assert Map.has_key?(first_cluster, :intensity)
    end

    test "groups nearby packets into single cluster" do
      # Two very close packets
      packets = [
        %Packet{id: 1, lat: Decimal.new("40.7128"), lon: Decimal.new("-74.0060")},
        %Packet{id: 2, lat: Decimal.new("40.7130"), lon: Decimal.new("-74.0058")}
      ]

      result = Clustering.cluster_packets(packets, 5, %{})
      assert {:heat_map, clusters} = result
      assert length(clusters) == 1
      assert hd(clusters).intensity == 2
    end

    test "separates distant packets into different clusters" do
      # Two far apart packets
      packets = [
        %Packet{id: 1, lat: Decimal.new("40.7128"), lon: Decimal.new("-74.0060")},
        %Packet{id: 2, lat: Decimal.new("51.5074"), lon: Decimal.new("-0.1278")}
      ]

      result = Clustering.cluster_packets(packets, 5, %{})
      assert {:heat_map, clusters} = result
      assert length(clusters) == 2
      assert Enum.all?(clusters, &(&1.intensity == 1))
    end

    test "handles packets with nil coordinates" do
      packets = [
        %Packet{id: 1, lat: nil, lon: nil},
        %Packet{id: 2, lat: Decimal.new("40.7128"), lon: Decimal.new("-74.0060")},
        %Packet{id: 3, lat: Decimal.new("40.7130"), lon: nil}
      ]

      result = Clustering.cluster_packets(packets, 5, %{})
      assert {:heat_map, clusters} = result
      # Only one valid packet should be clustered
      assert length(clusters) == 1
    end

    test "cluster radius decreases with zoom level" do
      # These points are ~0.014 degrees apart (about 1.5km)
      packets = [
        %Packet{id: 1, lat: Decimal.new("40.7128"), lon: Decimal.new("-74.0060")},
        %Packet{id: 2, lat: Decimal.new("40.7228"), lon: Decimal.new("-74.0160")}
      ]

      # At zoom 3, these should be one cluster (radius 1.25 degrees)
      {:heat_map, zoom3_clusters} = Clustering.cluster_packets(packets, 3, %{})
      assert length(zoom3_clusters) == 1

      # At zoom 8, these should be two clusters (radius 0.039 degrees)
      # but our points are only 0.014 degrees apart, so they'll still cluster
      # Let's use points that are further apart
      far_packets = [
        %Packet{id: 1, lat: Decimal.new("40.7128"), lon: Decimal.new("-74.0060")},
        %Packet{id: 2, lat: Decimal.new("40.8128"), lon: Decimal.new("-74.1060")}
      ]

      # At zoom 7, these should be two clusters (0.1 degree apart > 0.078 radius)
      {:heat_map, zoom7_clusters} = Clustering.cluster_packets(far_packets, 7, %{})
      assert length(zoom7_clusters) == 2
    end
  end
end

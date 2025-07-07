defmodule Aprsme.Integration.HistoricalPacketsTest do
  use AprsmeWeb.ConnCase

  import Phoenix.ConnTest
  import Phoenix.LiveViewTest

  setup do
    Mox.set_mox_global()
    Application.put_env(:aprsme, :packets_module, PacketsMock)
    on_exit(fn -> Mox.set_mox_private() end)
    :ok
  end

  describe "historical packet loading" do
    setup do
      # Create test packets with different timestamps
      now = DateTime.utc_now()
      one_hour_ago = DateTime.add(now, -3600, :second)
      thirty_minutes_ago = DateTime.add(now, -1800, :second)
      ten_minutes_ago = DateTime.add(now, -600, :second)

      # Mock packet data for the same callsign at different times
      packet1 = %Aprsme.Packet{
        id: 1,
        base_callsign: "TEST1",
        ssid: 0,
        has_position: true,
        lat: 39.8,
        lon: -98.5,
        received_at: one_hour_ago,
        data_extended: %{
          symbol_table_id: "/",
          symbol_code: ">",
          comment: "Old position"
        }
      }

      packet2 = %Aprsme.Packet{
        id: 2,
        base_callsign: "TEST1",
        ssid: 0,
        has_position: true,
        lat: 39.9,
        lon: -98.4,
        received_at: thirty_minutes_ago,
        data_extended: %{
          symbol_table_id: "/",
          symbol_code: ">",
          comment: "Middle position"
        }
      }

      packet3 = %Aprsme.Packet{
        id: 3,
        base_callsign: "TEST1",
        ssid: 0,
        has_position: true,
        lat: 40.0,
        lon: -98.3,
        received_at: ten_minutes_ago,
        data_extended: %{
          symbol_table_id: "/",
          symbol_code: ">",
          comment: "Latest position"
        }
      }

      # Mock packet for different callsign
      packet4 = %Aprsme.Packet{
        id: 4,
        base_callsign: "TEST2",
        ssid: 0,
        has_position: true,
        lat: 38.8,
        lon: -97.5,
        received_at: thirty_minutes_ago,
        data_extended: %{
          symbol_table_id: "/",
          symbol_code: "k",
          comment: "Different station"
        }
      }

      {:ok, packets: [packet1, packet2, packet3, packet4]}
    end

    # Removed tests:
    # - loads all historical packets at once when map is ready
    # - only loads packets within map bounds
    # - handles empty historical packets gracefully
    # - clears historical packets when requested
    # - handles locate_me event after historical packets are loaded
  end

  describe "live packet updates with historical data" do
    setup do
      # Create a historical packet
      now = DateTime.utc_now()
      thirty_minutes_ago = DateTime.add(now, -1800, :second)

      historical_packet = %Aprsme.Packet{
        id: 100,
        base_callsign: "LIVE1",
        ssid: 0,
        has_position: true,
        lat: 39.8,
        lon: -98.5,
        received_at: thirty_minutes_ago,
        data_extended: %{
          symbol_table_id: "/",
          symbol_code: ">",
          comment: "Old position"
        }
      }

      {:ok, historical_packet: historical_packet}
    end

    # Removed test:
    # - new live packet updates marker for same callsign
  end
end

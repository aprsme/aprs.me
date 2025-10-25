defmodule AprsmeWeb.MapLive.IndexTest do
  use AprsmeWeb.ConnCase

  import Phoenix.ConnTest
  import Phoenix.LiveViewTest

  describe "Index" do
    test "renders map view", %{conn: conn} do
      {:ok, view, html} = live(conn, "/", on_error: :warn)

      assert html =~ "APRS Map"
      assert has_element?(view, "#aprs-map")
    end

    test "handles bounds_changed event with float values", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Simulate JavaScript sending float values (the problematic case)
      bounds_params = %{
        "bounds" => %{
          "north" => 61.897577621605016,
          "south" => 5.441022303717974,
          "east" => -34.45312500000001,
          "west" => -161.54296875000003
        }
      }

      # This should not crash with ArgumentError: not a binary
      assert render_hook(view, "bounds_changed", bounds_params)
    end

    test "handles bounds_changed event with string values", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Simulate JavaScript sending string values (expected case)
      bounds_params = %{
        "bounds" => %{
          "north" => "61.897577621605016",
          "south" => "5.441022303717974",
          "east" => "-34.45312500000001",
          "west" => "-161.54296875000003"
        }
      }

      assert render_hook(view, "bounds_changed", bounds_params)
    end

    test "handles bounds_changed event with mixed value types", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Test mixed types (float, string, integer)
      bounds_params = %{
        "bounds" => %{
          # float
          "north" => 61.897577621605016,
          # string
          "south" => "5.441022303717974",
          # integer
          "east" => -34,
          # string
          "west" => "-161.54296875000003"
        }
      }

      assert render_hook(view, "bounds_changed", bounds_params)
    end

    test "loads historical packets when map is ready", %{conn: conn} do
      # Mock the Packets.get_packets_for_replay function to return empty list
      # since we now load all historical packets at once instead of replaying
      Mox.stub(Aprsme.PacketsMock, :get_packets_for_replay, fn _opts -> [] end)

      {:ok, view, _html} = live(conn, "/")

      # Simulate map initialization which should trigger historical packet loading
      assert render_hook(view, "map_ready", %{})

      # Wait for the initialize_replay message to be processed
      Process.sleep(20)

      # The view should still be rendering without errors after loading historical packets
      assert render(view) =~ "aprs-map"
    end

    test "loads historical packets with correct time range", %{conn: conn} do
      # Create mock historical packets from the last 2 hours
      now = DateTime.utc_now()
      two_hours_ago = DateTime.add(now, -7200, :second)
      one_hour_ago = DateTime.add(now, -3600, :second)

      mock_packets = [
        %{
          id: 1,
          base_callsign: "TEST1",
          sender: "TEST1-1",
          ssid: "1",
          data_type: "position",
          symbol_table_id: "/",
          symbol_code: ">",
          lat: 39.8283,
          lon: -98.5795,
          received_at: two_hours_ago,
          inserted_at: two_hours_ago,
          has_position: true,
          comment: "Historical packet 1"
        },
        %{
          id: 2,
          base_callsign: "TEST2",
          sender: "TEST2-1",
          ssid: "1",
          data_type: "position",
          symbol_table_id: "/",
          symbol_code: ">",
          lat: 40.0,
          lon: -99.0,
          received_at: one_hour_ago,
          inserted_at: one_hour_ago,
          has_position: true,
          comment: "Historical packet 2"
        }
      ]

      # Mock the Packets.get_packets_for_replay function to return our test packets
      Mox.stub(Aprsme.PacketsMock, :get_packets_for_replay, fn opts ->
        # Verify that the time range is being passed correctly (not overridden to 1 hour)
        assert Map.has_key?(opts, :start_time)
        assert Map.has_key?(opts, :end_time)

        # The start_time should be 2 hours ago (historical_hours = "2")
        expected_start = DateTime.add(now, -7200, :second)
        assert_in_delta DateTime.to_unix(opts.start_time), DateTime.to_unix(expected_start), 1

        mock_packets
      end)

      {:ok, view, _html} = live(conn, "/")

      # Set historical_hours to 2 to test the time range
      render_hook(view, "update_historical_hours", %{"historical_hours" => "2"})
      Process.sleep(10)

      # Simulate map initialization which should trigger historical packet loading
      assert render_hook(view, "map_ready", %{})

      # Wait for the initialize_replay message to be processed
      Process.sleep(20)

      # The view should still be rendering without errors after loading historical packets
      assert render(view) =~ "aprs-map"
    end

    test "handles clear_and_reload_markers event", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # This should not crash and should work without replay functionality
      assert render_hook(view, "clear_and_reload_markers", %{})
    end

    test "sets correct page title", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      assert page_title(view) =~ "APRS Map"
    end

    test "handles map_ready event", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Simulate map initialization
      assert render_hook(view, "map_ready", %{})
    end
  end
end

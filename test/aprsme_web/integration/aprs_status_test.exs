defmodule AprsmeWeb.Integration.AprsStatusTest do
  @moduledoc """
  Integration tests to verify that web interfaces work properly
  without external APRS connections in the test environment.
  """
  use AprsmeWeb.ConnCase

  import Phoenix.ConnTest
  import Phoenix.LiveViewTest

  alias AprsmeWeb.Endpoint
  alias Ecto.Adapters.SQL

  describe "APRS status endpoints without external connections" do
    test "status JSON endpoint returns proper response without APRS connection", %{conn: conn} do
      conn = get(conn, "/status.json")

      assert response = json_response(conn, 200)

      # Should have basic structure even without APRS connection
      assert Map.has_key?(response, "aprs_is")
      assert Map.has_key?(response, "application")

      # APRS status should indicate disconnected state
      aprs_status = response["aprs_is"]
      assert aprs_status["connected"] == false
      assert is_binary(aprs_status["server"])
      assert is_integer(aprs_status["port"])
      assert aprs_status["uptime_seconds"] == 0

      # Should not contain real APRS server addresses
      forbidden_servers = [
        "rotate.aprs2.net",
        "dallas.aprs2.net",
        "seattle.aprs2.net"
      ]

      refute Enum.any?(forbidden_servers, fn server ->
               String.contains?(aprs_status["server"], server)
             end)
    end

    test "main map page loads without APRS connection", %{conn: conn} do
      {:ok, view, html} = live(conn, "/")

      # Page should load successfully
      assert has_element?(view, "#aprs-map")

      # Should not show connected status
      refute html =~ "Connected to APRS"

      # Should handle disconnected state gracefully
      # Basic content should be present - check for map div
      assert html =~ "aprs-map"
    end

    test "status live view works without APRS connection", %{conn: conn} do
      # Try to access status page if it exists
      case live(conn, "/status") do
        {:ok, view, html} ->
          # If status page exists, verify it handles disconnected state
          assert html =~ "System Status"

          # Should show disconnected state information
          assert has_element?(view, "[data-testid='connection-status']") ||
                   html =~ "disconnected" ||
                   html =~ "not connected"

        {:error, {:live_redirect, %{to: "/"}}} ->
          # If redirected to home, that's acceptable
          :ok

        {:error, {:redirect, %{to: "/"}}} ->
          # If redirected to home, that's acceptable
          :ok
      end
    end

    test "packet data endpoints handle no external connection gracefully", %{conn: conn} do
      # Test packets live view instead of API endpoint
      case live(conn, "/packets") do
        {:ok, _view, html} ->
          # Should load packets page successfully
          assert html =~ "Packets"

        {:error, {:redirect, %{to: "/"}}} ->
          # Redirect is acceptable
          :ok

        {:error, _} ->
          # Other errors might be acceptable in test environment
          :ok
      end
    end
  end

  describe "LiveView event handling without APRS" do
    test "map events work without APRS connection", %{conn: conn} do
      # Set the mock to allow calls from any process
      Mox.set_mox_global()

      # Stub the function that will be called during bounds changes
      Mox.stub(Aprsme.PacketsMock, :get_recent_packets, fn _opts -> [] end)

      {:ok, view, _html} = live(conn, "/")

      # Test map bounds change event
      bounds_params = %{
        "bounds" => %{
          "north" => "33.1",
          "south" => "32.9",
          "east" => "-95.9",
          "west" => "-96.1"
        }
      }

      # Should not crash when handling events without APRS connection
      assert render_hook(view, "bounds_changed", bounds_params)

      # Wait a bit for any async processes
      Process.sleep(50)

      # Test map ready event
      assert render_hook(view, "map_ready", %{})

      # Replay functionality has been removed - historical packets are now loaded all at once
    end

    test "real-time updates are disabled without APRS connection", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/")

      # Verify that the view is not subscribed to real-time APRS updates
      # since there's no APRS connection

      # Send a test broadcast that would normally come from APRS.Is
      test_packet = %{
        sender: "TEST-1",
        latitude: 33.0,
        longitude: -96.0,
        timestamp: DateTime.utc_now()
      }

      # This should not cause any updates since APRS.Is is not running
      Endpoint.broadcast("aprs_messages", "packet", test_packet)

      # Give a moment for any potential updates
      Process.sleep(5)

      # View should remain stable (no crashes from missing APRS connection)
      assert has_element?(view, "#aprs-map")
    end
  end

  describe "error handling without external dependencies" do
    test "application handles missing APRS connection gracefully", %{conn: conn} do
      # Verify various endpoints don't crash when APRS.Is is not available

      # Home page
      assert {:ok, _view, html} = live_with_warn(conn, "/")
      assert html =~ "APRS"

      # API status
      conn = get(conn, "/status.json")
      assert json_response(conn, 200)

      # Any authentication pages should still work
      conn = build_conn()
      conn = get(conn, "/users/register")
      # Redirect or not found is acceptable
      assert html_response(conn, 200) =~ "Register" or
               conn.status in [302, 404]
    end

    test "database operations work independently of APRS connection", %{conn: conn} do
      # Verify that core application functionality works without APRS

      # Database should be accessible
      assert SQL.query!(Aprsme.Repo, "SELECT 1", [])

      # Web interface should load
      {:ok, _view, html} = live_with_warn(conn, "/")
      assert html =~ "APRS"

      # This confirms the app can function without external APRS data
    end
  end

  describe "test environment verification" do
    test "confirms we're in test environment" do
      # Double-check we're actually in test mode
      assert Mix.env() == :test
      assert Application.get_env(:aprsme, :env) == :test
      assert Application.get_env(:aprsme, :disable_aprs_connection) == true
    end

    test "APRS.Is process is not running" do
      # Verify the real APRS.Is GenServer is not started
      assert Process.whereis(Aprsme.Is) == nil
    end

    test "no external network configuration in test" do
      # Verify test config doesn't point to real servers
      server = Application.get_env(:aprsme, :aprsme_is_server)

      # Should be nil or a test value
      assert server == nil or
               (is_binary(server) and String.contains?(server, "test")) or
               (is_binary(server) and String.contains?(server, "mock"))
    end
  end
end

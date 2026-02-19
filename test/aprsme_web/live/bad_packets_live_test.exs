defmodule AprsmeWeb.BadPacketsLiveTest do
  use AprsmeWeb.ConnCase

  import Phoenix.LiveViewTest

  alias Aprsme.BadPacket
  alias Aprsme.Repo

  describe "Index" do
    test "renders bad packets page with card", %{conn: conn} do
      {:ok, _index_live, html} = live(conn, ~p"/badpackets", on_error: :warn)

      assert html =~ "shadow-sm"
      assert html =~ "sm:rounded-lg"
    end

    test "lists all bad packets with table", %{conn: conn} do
      bad_packet =
        Repo.insert!(%BadPacket{
          raw_packet: "KD9PDP>APRS:Invalid packet data",
          error_message: "Failed to parse APRS data",
          error_type: "parse_error",
          attempted_at: DateTime.utc_now()
        })

      {:ok, _index_live, html} = live(conn, ~p"/badpackets", on_error: :warn)

      assert html =~ bad_packet.error_message
      assert html =~ bad_packet.error_type
      assert html =~ "table"
      assert html =~ "rounded-md"
    end

    test "displays empty state when no bad packets", %{conn: conn} do
      {:ok, _index_live, html} = live(conn, ~p"/badpackets", on_error: :warn)

      assert html =~ "No bad packets"
      assert html =~ "All packets are parsing successfully!"
      assert html =~ "text-green-600"
    end

    test "updates in real-time when new bad packet is created", %{conn: conn} do
      {:ok, index_live, _html} = live(conn, ~p"/badpackets", on_error: :warn)

      # Create a bad packet after the live view is loaded
      bad_packet =
        Repo.insert!(%BadPacket{
          raw_packet: "KD9PDP>APRS:New invalid packet",
          error_message: "New parse error",
          error_type: "validation_error",
          attempted_at: DateTime.utc_now()
        })

      # The LiveView listens to postgres notifications, so we need to trigger a refresh
      # by sending the :do_refresh message directly
      send(index_live.pid, :do_refresh)

      # Wait for the async update
      :timer.sleep(100)

      html = render(index_live)
      assert html =~ bad_packet.error_message
      assert html =~ bad_packet.error_type
    end

    test "works correctly with dark mode support", %{conn: conn} do
      {:ok, _index_live, html} = live(conn, ~p"/badpackets", on_error: :warn)

      assert html =~ "dark:bg-gray-800"
      assert html =~ "dark:text-gray-400"
    end
  end
end

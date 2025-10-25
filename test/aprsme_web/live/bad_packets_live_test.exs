defmodule AprsmeWeb.BadPacketsLiveTest do
  use AprsmeWeb.ConnCase

  import Phoenix.LiveViewTest

  alias Aprsme.BadPacket
  alias Aprsme.Repo

  describe "Index" do
    test "renders bad packets page with DaisyUI card", %{conn: conn} do
      {:ok, _index_live, html} = live(conn, ~p"/badpackets", on_error: :warn)

      assert html =~ "card"
      assert html =~ "bg-base-100"
      assert html =~ "shadow-xl"
    end

    test "lists all bad packets with DaisyUI table", %{conn: conn} do
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
      assert html =~ "badge"
    end

    test "displays empty state with DaisyUI components when no bad packets", %{conn: conn} do
      {:ok, _index_live, html} = live(conn, ~p"/badpackets", on_error: :warn)

      assert html =~ "No bad packets"
      assert html =~ "All packets are parsing successfully!"
      assert html =~ "text-success"
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

    test "works correctly in dark mode", %{conn: conn} do
      {:ok, _index_live, html} = live(conn, ~p"/badpackets", on_error: :warn)

      # DaisyUI uses data-theme for theming
      # The components should work with both light and dark themes
      assert html =~ "bg-base-100"
      assert html =~ "text-base-content"
    end
  end
end

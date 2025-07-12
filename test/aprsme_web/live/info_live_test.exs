defmodule AprsmeWeb.InfoLiveTest do
  use AprsmeWeb.ConnCase

  import Phoenix.LiveViewTest

  alias Aprsme.Packet
  alias Aprsme.Repo

  describe "show" do
    test "displays comment field when packet has comment", %{conn: conn} do
      {:ok, packet} =
        Repo.insert(%Packet{
          sender: "TEST-1",
          base_callsign: "TEST",
          ssid: "1",
          lat: Decimal.new("42.3601"),
          lon: Decimal.new("-71.0589"),
          comment: "Mobile station on the move",
          received_at: DateTime.truncate(DateTime.utc_now(), :second),
          has_position: true,
          location: %Geo.Point{coordinates: {-71.0589, 42.3601}}
        })

      {:ok, _view, html} = live(conn, ~p"/info/#{packet.sender}")

      assert html =~ "Comment"
      assert html =~ "Mobile station on the move"
    end

    test "does not display comment field when packet has no comment", %{conn: conn} do
      {:ok, packet} =
        Repo.insert(%Packet{
          sender: "TEST-2",
          base_callsign: "TEST",
          ssid: "2",
          lat: Decimal.new("42.3601"),
          lon: Decimal.new("-71.0589"),
          comment: nil,
          received_at: DateTime.truncate(DateTime.utc_now(), :second),
          has_position: true,
          location: %Geo.Point{coordinates: {-71.0589, 42.3601}}
        })

      {:ok, _view, html} = live(conn, ~p"/info/#{packet.sender}")

      refute html =~ "Comment"
    end

    test "displays comment with special characters", %{conn: conn} do
      {:ok, packet} =
        Repo.insert(%Packet{
          sender: "TEST-3",
          base_callsign: "TEST",
          ssid: "3",
          lat: Decimal.new("42.3601"),
          lon: Decimal.new("-71.0589"),
          comment: "Test comment with special chars: & < > \"",
          received_at: DateTime.truncate(DateTime.utc_now(), :second),
          has_position: true,
          location: %Geo.Point{coordinates: {-71.0589, 42.3601}}
        })

      {:ok, _view, html} = live(conn, ~p"/info/#{packet.sender}")

      assert html =~ "Comment"
      assert html =~ "Test comment with special chars"
      # Phoenix will HTML encode special characters automatically
    end
  end
end

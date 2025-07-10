defmodule AprsmeWeb.MapLive.GeolocationTest do
  use AprsmeWeb.ConnCase
  import Phoenix.LiveViewTest
  import Mox

  setup :verify_on_exit!

  describe "mount with IP geolocation" do
    test "uses IP geolocation when available in session", %{conn: conn} do
      # Add geolocation data to session
      conn =
        conn
        |> init_test_session(%{
          "ip_geolocation" => %{"lat" => 37.4056, "lng" => -122.0775}
        })

      {:ok, _view, html} = live(conn, "/")

      # Verify the map is initialized with the geolocation data
      assert html =~ "data-center"
      assert html =~ "37.4056"
      assert html =~ "-122.0775"
      assert html =~ "data-zoom=\"12\""
    end

    test "uses default center when no geolocation in session", %{conn: conn} do
      {:ok, _view, html} = live(conn, "/")

      # Verify default center is used
      assert html =~ "data-center"
      assert html =~ "39.8283"
      assert html =~ "-98.5795"
      assert html =~ "data-zoom=\"5\""
    end

    test "URL parameters override IP geolocation", %{conn: conn} do
      # Add geolocation data to session
      conn =
        conn
        |> init_test_session(%{
          "ip_geolocation" => %{"lat" => 37.4056, "lng" => -122.0775}
        })

      # Visit with URL parameters
      {:ok, _view, html} = live(conn, "/?lat=40.7128&lng=-74.0060&z=10")

      # Verify URL params take precedence
      assert html =~ "40.7128"
      assert html =~ "-74.0060"
      assert html =~ "data-zoom=\"10\""
    end

    test "handles invalid geolocation data gracefully", %{conn: conn} do
      # Add invalid geolocation data to session
      conn =
        conn
        |> init_test_session(%{
          "ip_geolocation" => %{"lat" => "invalid", "lng" => "invalid"}
        })

      {:ok, _view, html} = live(conn, "/")

      # Should fall back to defaults
      assert html =~ "39.8283"
      assert html =~ "-98.5795"
      assert html =~ "data-zoom=\"5\""
    end

    test "handles missing lat/lng in geolocation data", %{conn: conn} do
      # Add incomplete geolocation data
      conn =
        conn
        |> init_test_session(%{
          "ip_geolocation" => %{"city" => "Mountain View"}
        })

      {:ok, _view, html} = live(conn, "/")

      # Should fall back to defaults
      assert html =~ "39.8283"
      assert html =~ "-98.5795"
      assert html =~ "data-zoom=\"5\""
    end
  end
end
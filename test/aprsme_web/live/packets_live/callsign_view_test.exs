defmodule AprsmeWeb.PacketsLive.CallsignViewTest do
  use AprsmeWeb.ConnCase

  import Phoenix.LiveViewTest

  describe "live packet broadcast" do
    test "handles raw broadcast packet without :data key", %{conn: conn} do
      {:ok, view, _html} = live(conn, "/packets/DB0WUN-13")

      # Simulate a raw broadcast payload (as it arrives from the APRS pipeline)
      # This map does NOT have a :data key — only %Packet{} structs do
      raw_payload = %{
        sender: "DB0WUN-13",
        data_type: :weather,
        destination: "APN000",
        path: "TCPIP*,qAC,T2EISBERG",
        base_callsign: "DB0WUN",
        ssid: "13",
        received_at: DateTime.utc_now(),
        latitude: Decimal.new("50.0001"),
        longitude: Decimal.new("12.1393"),
        temperature: 31,
        humidity: 89,
        wind_speed: 3,
        comment: "232/003g009t031r000p000P000b10215h89L000eMB63",
        device_identifier: "APN000",
        data_extended: %{
          data_type: :weather,
          comment: "232/003g009t031r000p000P000b10215h89L000eMB63",
          latitude: Decimal.new("50.0001"),
          longitude: Decimal.new("12.1393"),
          symbol_code: "_",
          symbol_table_id: "/"
        }
      }

      AprsmeWeb.Endpoint.broadcast!("aprs_messages", "packet", raw_payload)

      # Should render without crashing
      html = render(view)
      assert html =~ "DB0WUN-13"
    end
  end
end

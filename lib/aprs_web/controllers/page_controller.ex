defmodule AprsWeb.PageController do
  use AprsWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end

  def map(conn, _params) do
    # This serves the legacy JavaScript-based map at /old
    render(conn, :map)
  end

  def packets(conn, _params) do
    render(conn, :packets)
  end

  def health(conn, _params) do
    # Check database connection
    db_healthy =
      try do
        Aprs.Repo.query!("SELECT 1")
        true
      rescue
        _ -> false
      end

    # Get application version
    version = :aprs |> Application.spec(:vsn) |> List.to_string()

    if db_healthy do
      json(conn, %{
        status: "ok",
        version: version,
        database: "connected",
        timestamp: DateTime.utc_now()
      })
    else
      conn
      |> put_status(503)
      |> json(%{
        status: "error",
        version: version,
        database: "disconnected",
        timestamp: DateTime.utc_now()
      })
    end
  end

  def ready(conn, _params) do
    # Simple readiness check without database dependency
    # This is faster for startup health checks
    version = :aprs |> Application.spec(:vsn) |> List.to_string()

    json(conn, %{
      status: "ready",
      version: version,
      timestamp: DateTime.utc_now()
    })
  end

  def status_json(conn, _params) do
    # Get APRS-IS connection status
    aprs_status = Aprs.Is.get_status()

    # Get application version
    version = :aprs |> Application.spec(:vsn) |> List.to_string()

    # Calculate uptime in a human-readable format
    uptime_display = format_uptime(aprs_status.uptime_seconds)

    json(conn, %{
      aprs_is: %{
        connected: aprs_status.connected,
        server: aprs_status.server,
        port: aprs_status.port,
        connected_at: aprs_status.connected_at,
        uptime_seconds: aprs_status.uptime_seconds,
        uptime_display: uptime_display,
        login_id: aprs_status.login_id,
        filter: aprs_status.filter
      },
      application: %{
        version: version,
        timestamp: DateTime.utc_now()
      }
    })
  end

  def test_map(conn, _params) do
    # Simple HTML test page for basic map functionality
    html(conn, """
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Map Test - APRS.me</title>
        <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"
              integrity="sha256-p4NxAoJBhIIN+hmNHrzRCf9tD/miZyoHS5obTRR9BMY=" crossorigin=""/>
        <style>
            body { margin: 0; padding: 0; font-family: Arial, sans-serif; }
            #map { height: 100vh; width: 100%; }
            .status { position: fixed; top: 10px; left: 10px; z-index: 1000; background: white; padding: 10px; border-radius: 5px; border: 1px solid #ccc; }
        </style>
    </head>
    <body>
        <div class="status">
            <strong>Map Test</strong><br>
            Status: <span id="status">Initializing...</span><br>
            <button onclick="testMarker()">Add Test Marker</button><br>
            <a href="/">← Back to Main Map</a>
        </div>
        <div id="map"></div>

        <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"
                integrity="sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo=" crossorigin=""></script>
        <script>
            let map;
            let status = document.getElementById('status');

            try {
                status.textContent = 'Loading Leaflet...';

                if (typeof L === 'undefined') {
                    throw new Error('Leaflet not loaded');
                }

                status.textContent = 'Initializing map...';

                map = L.map('map').setView([39.8283, -98.5795], 5);

                L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
                    attribution: '© OpenStreetMap contributors'
                }).addTo(map);

                status.textContent = 'Map loaded successfully!';
                status.style.color = 'green';

                // Add a default marker
                L.marker([39.8283, -98.5795])
                    .addTo(map)
                    .bindPopup('Test marker - map is working!')
                    .openPopup();

            } catch (error) {
                status.textContent = 'Error: ' + error.message;
                status.style.color = 'red';
                console.error('Map initialization error:', error);
            }

            function testMarker() {
                if (map) {
                    const lat = 39 + Math.random() * 10;
                    const lng = -100 + Math.random() * 10;
                    L.marker([lat, lng])
                        .addTo(map)
                        .bindPopup('Random test marker');
                    status.textContent = 'Added marker at ' + lat.toFixed(2) + ', ' + lng.toFixed(2);
                }
            }
        </script>
    </body>
    </html>
    """)
  end

  defp format_uptime(seconds) when seconds <= 0, do: "Not connected"

  defp format_uptime(seconds) do
    days = div(seconds, 86_400)
    hours = div(rem(seconds, 86_400), 3600)
    minutes = div(rem(seconds, 3600), 60)
    secs = rem(seconds, 60)

    cond do
      days > 0 -> "#{days}d #{hours}h #{minutes}m #{secs}s"
      hours > 0 -> "#{hours}h #{minutes}m #{secs}s"
      minutes > 0 -> "#{minutes}m #{secs}s"
      true -> "#{secs}s"
    end
  end
end

defmodule AprsWeb.MapLive.Debug do
  @moduledoc false
  use AprsWeb, :live_view

  @default_center %{lat: 39.8283, lng: -98.5795}
  @default_zoom 5

  def mount(_params, _session, socket) do
    socket =
      assign(socket,
        page_title: "Map Debug",
        map_center: @default_center,
        map_zoom: @default_zoom,
        debug_info: %{
          leaflet_loaded: false,
          map_initialized: false,
          hook_mounted: false,
          map_ready: false,
          element_dimensions: nil,
          errors: []
        }
      )

    {:ok, socket}
  end

  def handle_event("debug_info", params, socket) do
    debug_info = Map.merge(socket.assigns.debug_info, params)
    {:noreply, assign(socket, debug_info: debug_info)}
  end

  def handle_event("map_ready", _params, socket) do
    debug_info = Map.put(socket.assigns.debug_info, :map_ready, true)
    {:noreply, assign(socket, debug_info: debug_info)}
  end

  def render(assigns) do
    ~H"""
    <link
      rel="stylesheet"
      href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"
      integrity="sha256-p4NxAoJBhIIN+hmNHrzRCf9tD/miZyoHS5obTRR9BMY="
      crossorigin=""
    />
    <script
      src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"
      integrity="sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo="
      crossorigin=""
    >
    </script>

    <style>
      .debug-container {
        display: flex;
        height: 100vh;
      }

      .debug-panel {
        width: 400px;
        background: #f8f9fa;
        border-right: 1px solid #dee2e6;
        padding: 20px;
        overflow-y: auto;
        font-family: monospace;
        font-size: 12px;
      }

      .map-container {
        flex: 1;
        position: relative;
      }

      #debug-map {
        width: 100%;
        height: 100%;
      }

      .status-item {
        margin: 10px 0;
        padding: 8px;
        border-radius: 4px;
        border: 1px solid #ccc;
      }

      .status-ok {
        background: #d4edda;
        border-color: #c3e6cb;
        color: #155724;
      }

      .status-error {
        background: #f8d7da;
        border-color: #f5c6cb;
        color: #721c24;
      }

      .status-pending {
        background: #fff3cd;
        border-color: #ffeaa7;
        color: #856404;
      }

      .debug-title {
        font-size: 16px;
        font-weight: bold;
        margin-bottom: 20px;
        color: #333;
      }

      .test-button {
        background: #007cba;
        color: white;
        border: none;
        padding: 8px 16px;
        border-radius: 4px;
        cursor: pointer;
        margin: 5px 5px 5px 0;
        font-size: 12px;
      }

      .test-button:hover {
        background: #005a87;
      }
    </style>

    <div class="debug-container">
      <div class="debug-panel">
        <div class="debug-title">Map Debug Panel</div>

        <div class="status-item">
          <strong>Map Dimensions:</strong> <br />
          <span id="dimensions-info">Checking...</span>
        </div>

        <div class={"status-item #{if @debug_info.leaflet_loaded, do: "status-ok", else: "status-pending"}"}>
          <strong>Leaflet Library:</strong> <br />
          {if @debug_info.leaflet_loaded, do: "✓ Loaded", else: "⏳ Loading..."}
        </div>

        <div class={"status-item #{if @debug_info.hook_mounted, do: "status-ok", else: "status-pending"}"}>
          <strong>Phoenix Hook:</strong> <br />
          {if @debug_info.hook_mounted, do: "✓ Mounted", else: "⏳ Waiting..."}
        </div>

        <div class={"status-item #{if @debug_info.map_initialized, do: "status-ok", else: "status-pending"}"}>
          <strong>Map Initialization:</strong> <br />
          {if @debug_info.map_initialized, do: "✓ Initialized", else: "⏳ Waiting..."}
        </div>

        <div class={"status-item #{if @debug_info.map_ready, do: "status-ok", else: "status-pending"}"}>
          <strong>Map Ready:</strong> <br />
          {if @debug_info.map_ready, do: "✓ Ready", else: "⏳ Waiting..."}
        </div>

        <%= if length(@debug_info.errors) > 0 do %>
          <div class="status-item status-error">
            <strong>Errors:</strong>
            <br />
            <%= for error <- @debug_info.errors do %>
              • {error}<br />
            <% end %>
          </div>
        <% end %>

        <div class="status-item">
          <strong>Test Actions:</strong> <br />
          <button class="test-button" onclick="testLeaflet()">Test Leaflet</button>
          <button class="test-button" onclick="testMapSize()">Test Map Size</button>
          <button class="test-button" onclick="addTestMarker()">Add Marker</button>
          <button class="test-button" onclick="clearConsole()">Clear Console</button>
        </div>

        <div class="status-item">
          <strong>Console Output:</strong>
          <br />
          <div
            id="console-output"
            style="height: 200px; overflow-y: auto; background: white; padding: 10px; border: 1px solid #ccc; font-size: 10px;"
          >
          </div>
        </div>

        <div class="status-item">
          <strong>Instructions:</strong>
          <br /> 1. Open browser dev tools (F12)<br /> 2. Check Console tab for errors<br />
          3. Watch this panel for status updates<br /> 4. Use test buttons to debug issues
        </div>
      </div>

      <div class="map-container">
        <div
          id="debug-map"
          phx-hook="DebugMap"
          data-center={Jason.encode!(@map_center)}
          data-zoom={@map_zoom}
        >
        </div>
      </div>
    </div>

    <script>
      // Capture console output
      const originalLog = console.log;
      const originalError = console.error;
      const originalWarn = console.warn;

      function addToConsoleOutput(message, type = 'log') {
        const output = document.getElementById('console-output');
        if (output) {
          const timestamp = new Date().toLocaleTimeString();
          const color = type === 'error' ? 'red' : type === 'warn' ? 'orange' : 'black';
          output.innerHTML += `<div style="color: ${color};">[${timestamp}] ${message}</div>`;
          output.scrollTop = output.scrollHeight;
        }
      }

      console.log = function(...args) {
        originalLog.apply(console, args);
        addToConsoleOutput(args.join(' '), 'log');
      };

      console.error = function(...args) {
        originalError.apply(console, args);
        addToConsoleOutput(args.join(' '), 'error');
      };

      console.warn = function(...args) {
        originalWarn.apply(console, args);
        addToConsoleOutput(args.join(' '), 'warn');
      };

      function testLeaflet() {
        if (typeof L !== 'undefined') {
          console.log('✓ Leaflet is available, version:', L.version);
          window.liveSocket.pushEvent('debug_info', {leaflet_loaded: true});
        } else {
          console.error('✗ Leaflet is not available');
          window.liveSocket.pushEvent('debug_info', {leaflet_loaded: false, errors: ['Leaflet not loaded']});
        }
      }

      function testMapSize() {
        const mapEl = document.getElementById('debug-map');
        if (mapEl) {
          const rect = mapEl.getBoundingClientRect();
          console.log('Map element dimensions:', rect);
          document.getElementById('dimensions-info').textContent =
            `${rect.width}x${rect.height} (${rect.left},${rect.top})`;
        }
      }

      function addTestMarker() {
        if (window.debugMapInstance) {
          const marker = L.marker([39.8283, -98.5795])
            .addTo(window.debugMapInstance)
            .bindPopup('Test Marker');
          console.log('✓ Test marker added');
        } else {
          console.error('✗ No map instance available');
        }
      }

      function clearConsole() {
        document.getElementById('console-output').innerHTML = '';
      }

      // Test Leaflet availability when page loads
      setTimeout(testLeaflet, 1000);
      setTimeout(testMapSize, 1000);
    </script>
    """
  end
end

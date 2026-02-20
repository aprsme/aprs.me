defmodule AprsmeWeb.ApiDocsLive do
  @moduledoc """
  LiveView for API documentation page.
  """
  use AprsmeWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(page_title: "API Documentation")
     |> assign(test_callsign: "")
     |> assign(loading: false)
     |> assign(api_result: nil)
     |> assign(error: nil)}
  end

  @impl true
  def handle_event("update_callsign", %{"callsign" => callsign}, socket) do
    {:noreply,
     socket
     |> assign(test_callsign: callsign)
     |> assign(error: nil)}
  end

  @impl true
  def handle_event("test_api", %{"callsign" => callsign}, socket) do
    normalized_callsign = callsign |> String.trim() |> String.upcase()

    if normalized_callsign == "" do
      {:noreply, assign(socket, error: "Please enter a callsign")}
    else
      socket =
        socket
        |> assign(loading: true)
        |> assign(error: nil)
        |> assign(api_result: nil)

      # Make the API call
      send(self(), {:call_api, normalized_callsign})
      {:noreply, socket}
    end
  end

  @impl true
  def handle_info({:call_api, callsign}, socket) do
    case make_http_request(callsign) do
      {:ok, json_result} ->
        {:noreply,
         socket
         |> assign(loading: false)
         |> assign(api_result: json_result)}

      {:error, error_message} ->
        {:noreply,
         socket
         |> assign(loading: false)
         |> assign(error: error_message)}
    end
  end

  defp make_http_request(callsign) do
    alias Aprsme.Packets
    alias AprsmeWeb.TimeUtils

    # Validate callsign format (same as controller)
    callsign_regex = ~r/^[A-Z0-9]{1,3}[0-9][A-Z]{1,4}(-[0-9]{1,2})?$/

    if not String.match?(callsign, callsign_regex) or String.length(callsign) > 12 do
      response = %{
        "error" => %{
          "message" => "Invalid callsign format",
          "code" => "bad_request"
        }
      }

      {:ok, Jason.encode!(response, pretty: true)}
    else
      # Get packet data (same logic as controller)
      thirty_days_ago = TimeUtils.days_ago(30)

      opts = %{
        callsign: callsign,
        start_time: thirty_days_ago,
        limit: 1
      }

      case Packets.get_recent_packets(opts) do
        [] ->
          response = %{
            "data" => nil,
            "message" => "No packets found for callsign #{callsign}"
          }

          {:ok, Jason.encode!(response, pretty: true)}

        [packet | _] ->
          # Format packet data using the same logic as CallsignJSON
          packet_data = %{
            "id" => packet.id,
            "callsign" => format_callsign(packet.base_callsign, packet.ssid),
            "base_callsign" => packet.base_callsign,
            "ssid" => packet.ssid,
            "sender" => packet.sender,
            "destination" => packet.destination,
            "path" => packet.path,
            "data_type" => packet.data_type,
            "information_field" => get_in(packet.data || %{}, ["information_field"]),
            "raw_packet" => sanitize_raw_packet(packet.raw_packet),
            "received_at" => packet.received_at,
            "region" => packet.region,
            "position" => format_position(packet),
            "symbol" => format_symbol(packet),
            "comment" => packet.comment,
            "timestamp" => packet.timestamp,
            "aprs_messaging" => packet.aprs_messaging,
            "weather" => format_weather(packet),
            "equipment" => format_equipment(packet),
            "message" => format_message(packet),
            "has_position" => packet.has_position,
            "inserted_at" => packet.inserted_at,
            "updated_at" => packet.updated_at
          }

          response = %{"data" => packet_data}
          {:ok, Jason.encode!(response, pretty: true)}
      end
    end
  rescue
    error ->
      {:error, "Request error: #{inspect(error)}"}
  end

  defp format_callsign(base_callsign, nil), do: base_callsign
  defp format_callsign(base_callsign, "0"), do: base_callsign
  defp format_callsign(base_callsign, ssid), do: "#{base_callsign}-#{ssid}"

  defp format_position(%{has_position: false}), do: nil
  defp format_position(%{lat: nil, lon: nil}), do: nil

  defp format_position(packet) do
    base =
      %{
        "latitude" => to_float(packet.lat),
        "longitude" => to_float(packet.lon)
      }
      |> maybe_add("course", packet.course)
      |> maybe_add("speed", packet.speed)
      |> maybe_add("altitude", packet.altitude)

    if map_size(base) == 2, do: base, else: base
  end

  defp format_symbol(%{symbol_code: nil, symbol_table_id: nil}), do: nil

  defp format_symbol(packet) do
    %{
      "code" => packet.symbol_code,
      "table_id" => packet.symbol_table_id
    }
  end

  defp format_weather(packet) do
    weather =
      %{}
      |> maybe_add("temperature", packet.temperature)
      |> maybe_add("humidity", packet.humidity)
      |> maybe_add("wind_speed", packet.wind_speed)
      |> maybe_add("wind_direction", packet.wind_direction)
      |> maybe_add("wind_gust", packet.wind_gust)
      |> maybe_add("pressure", packet.pressure)
      |> maybe_add("rain_1h", packet.rain_1h)
      |> maybe_add("rain_24h", packet.rain_24h)
      |> maybe_add("rain_since_midnight", packet.rain_since_midnight)

    if map_size(weather) == 0, do: nil, else: weather
  end

  defp format_equipment(packet) do
    device =
      case packet.device_identifier do
        nil -> nil
        "" -> nil
        identifier -> Aprsme.DeviceCache.lookup_device(identifier)
      end

    equipment =
      %{}
      |> maybe_add("device_identifier", packet.device_identifier)
      |> maybe_add("manufacturer", packet.manufacturer)
      |> maybe_add("equipment_type", packet.equipment_type)
      |> maybe_add("device_model", device && device.model)
      |> maybe_add("device_vendor", device && device.vendor)
      |> maybe_add("device_contact", device && device.contact)
      |> maybe_add("device_class", device && device.class)

    if map_size(equipment) == 0, do: nil, else: equipment
  end

  defp format_message(%{addressee: nil, message_text: nil, message_number: nil}), do: nil

  defp format_message(packet) do
    message =
      %{}
      |> maybe_add("addressee", packet.addressee)
      |> maybe_add("text", packet.message_text)
      |> maybe_add("number", packet.message_number)

    if map_size(message) == 0, do: nil, else: message
  end

  defp maybe_add(map, _key, nil), do: map
  defp maybe_add(map, _key, ""), do: map
  defp maybe_add(map, key, value), do: Map.put(map, key, value)

  defp to_float(%Decimal{} = decimal), do: Decimal.to_float(decimal)
  defp to_float(value) when is_number(value), do: value
  defp to_float(_), do: nil

  defp sanitize_raw_packet(raw_packet) when is_binary(raw_packet) do
    Aprsme.EncodingUtils.sanitize_string(raw_packet)
  end

  defp sanitize_raw_packet(raw_packet), do: raw_packet

  @impl true
  def render(assigns) do
    ~H"""
    <div class="container mx-auto max-w-6xl py-8">
      <div class="mb-8">
        <h1 class="text-4xl font-bold mb-4">APRS.me API Documentation</h1>
        <p class="text-lg text-gray-500 dark:text-gray-400">
          RESTful JSON API for accessing APRS packet data and station information.
        </p>
      </div>
      
    <!-- API Overview -->
      <div class="bg-white shadow-sm sm:rounded-lg dark:bg-gray-800/50 dark:shadow-none dark:outline dark:-outline-offset-1 dark:outline-white/10 mb-8">
        <div class="px-6 py-8">
          <h2 class="text-2xl font-semibold text-gray-900 dark:text-white">Overview</h2>
          <p class="mb-4">
            The APRS.me API provides programmatic access to Amateur Radio APRS (Automatic Packet Reporting System)
            data. All API endpoints return JSON data and follow RESTful conventions.
          </p>

          <div class="grid md:grid-cols-2 gap-6 mt-6">
            <div class="bg-indigo-50 dark:bg-indigo-500/10 p-4 rounded-lg">
              <h3 class="font-semibold mb-2">Base URL</h3>
              <code class="text-sm bg-gray-100 dark:bg-gray-800 px-2 py-1 rounded">
                https://aprs.me/api/v1
              </code>
            </div>

            <div class="bg-green-50 dark:bg-green-500/10 p-4 rounded-lg">
              <h3 class="font-semibold mb-2">Content Type</h3>
              <code class="text-sm bg-gray-100 dark:bg-gray-800 px-2 py-1 rounded">
                application/json
              </code>
            </div>
          </div>

          <div class="rounded-md bg-yellow-50 p-4 dark:bg-yellow-500/10 mt-6">
            <div class="flex">
              <svg
                xmlns="http://www.w3.org/2000/svg"
                class="shrink-0 h-6 w-6 text-yellow-700 dark:text-yellow-400"
                fill="none"
                viewBox="0 0 24 24"
                stroke="currentColor"
              >
                <path
                  stroke-linecap="round"
                  stroke-linejoin="round"
                  stroke-width="2"
                  d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-2.5L13.732 4c-.77-.833-1.964-.833-2.732 0L3.732 16.5c-.77.833.192 2.5 1.732 2.5z"
                />
              </svg>
              <div class="ml-3">
                <h3 class="text-sm font-bold text-yellow-700 dark:text-yellow-400">Rate Limiting</h3>
                <div class="text-sm text-yellow-700 dark:text-yellow-400">
                  API requests are rate limited to 100 requests per minute per IP address. Exceeding this limit will return a 429 Too Many Requests response.
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
      
    <!-- API Endpoints -->
      <div class="space-y-8">
        <!-- Callsign Endpoint -->
        <div class="bg-white shadow-sm sm:rounded-lg dark:bg-gray-800/50 dark:shadow-none dark:outline dark:-outline-offset-1 dark:outline-white/10">
          <div class="px-6 py-8">
            <div class="flex items-center justify-between mb-4">
              <h2 class="text-xl font-semibold text-gray-900 dark:text-white">Get Latest Packet by Callsign</h2>
              <span class="inline-flex items-center rounded-md bg-green-100 px-2 py-1 text-xs font-medium text-green-700 dark:bg-green-400/10 dark:text-green-400">
                GET
              </span>
            </div>

            <div class="mb-4">
              <h3 class="text-lg font-medium mb-2">Endpoint</h3>
              <div class="bg-gray-100 dark:bg-gray-800 p-3 rounded-lg">
                <code class="text-sm">GET /api/v1/callsign/{"{callsign}"}</code>
              </div>
            </div>

            <div class="mb-4">
              <h3 class="text-lg font-medium mb-2">Description</h3>
              <p>
                Retrieves the most recent APRS packet for the specified callsign. The callsign can include
                an SSID (e.g., N0CALL-9) or just the base callsign (e.g., N0CALL).
              </p>
            </div>

            <div class="mb-4">
              <h3 class="text-lg font-medium mb-2">Parameters</h3>
              <div class="overflow-x-auto">
                <table class="min-w-full divide-y divide-gray-300 dark:divide-gray-700">
                  <thead class="text-left text-sm font-semibold text-gray-900 dark:text-white">
                    <tr>
                      <th class="px-3 py-3.5">Parameter</th>
                      <th class="px-3 py-3.5">Type</th>
                      <th class="px-3 py-3.5">Required</th>
                      <th class="px-3 py-3.5">Description</th>
                    </tr>
                  </thead>
                  <tbody class="divide-y divide-gray-200 dark:divide-gray-700">
                    <tr>
                      <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400 font-mono">
                        callsign
                      </td>
                      <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400">string</td>
                      <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400">Yes</td>
                      <td class="px-3 py-4 text-sm text-gray-500 dark:text-gray-400">
                        Amateur radio callsign with optional SSID (e.g., N0CALL or N0CALL-9)
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>

            <div class="mb-4">
              <h3 class="text-lg font-medium mb-2">Example Request</h3>
              <div class="bg-gray-900 text-gray-100 dark:bg-gray-950 p-4 rounded-lg overflow-x-auto">
                <pre><code>curl -X GET "https://aprs.me/api/v1/callsign/N0CALL-9" \
     -H "Accept: application/json"</code></pre>
              </div>
            </div>

            <div class="mb-4">
              <h3 class="text-lg font-medium mb-2">Response Format</h3>

              <h4 class="text-md font-medium mb-2">Success Response (200 OK)</h4>
              <div class="bg-gray-900 text-gray-100 dark:bg-gray-950 p-4 rounded-lg overflow-x-auto mb-4">
                <pre><code><%= raw ~s|{
    "data": {
    "id": "d7249877-d4a6-45c2-b314-2a8a355d2566",
    "callsign": "N0CALL-9",
    "base_callsign": "N0CALL",
    "ssid": "9",
    "sender": "N0CALL-9",
    "destination": "APRS",
    "path": "WIDE1-1,WIDE2-1",
    "data_type": "position",
    "information_field": "!4740.00N/12200.00W>Mobile Station",
    "raw_packet": "N0CALL-9>APRS,WIDE1-1,WIDE2-1:!4740.00N/12200.00W>Mobile Station",
    "received_at": "2024-01-15T10:30:45Z",
    "region": "US-West",
    "position": {
      "latitude": 47.666667,
      "longitude": -122.0,
      "course": 90,
      "speed": 35.5,
      "altitude": 152.4
    },
    "symbol": {
      "code": ">",
      "table_id": "/"
    },
    "comment": "Mobile Station",
    "timestamp": null,
    "aprs_messaging": false,
    "weather": null,
    "equipment": {
      "device_identifier": "APK004",
      "manufacturer": "Kenwood",
      "equipment_type": "TM-D710",
      "device_model": "TM-D710G",
      "device_vendor": "Kenwood",
      "device_class": "rig"
    },
    "message": null,
    "has_position": true,
    "inserted_at": "2024-01-15T10:30:45Z",
    "updated_at": "2024-01-15T10:30:45Z"
    }
    }| %></code></pre>
              </div>

              <h4 class="text-md font-medium mb-2">Not Found Response (404)</h4>
              <div class="bg-gray-900 text-gray-100 dark:bg-gray-950 p-4 rounded-lg overflow-x-auto mb-4">
                <pre><code><%= raw ~s|{
    "data": null,
    "message": "No packets found for callsign N0CALL-9"
    }| %></code></pre>
              </div>

              <h4 class="text-md font-medium mb-2">Error Response (400)</h4>
              <div class="bg-gray-900 text-gray-100 dark:bg-gray-950 p-4 rounded-lg overflow-x-auto">
                <pre><code><%= raw ~s|{
    "error": {
    "message": "Invalid callsign format",
    "code": "bad_request"
    }
    }| %></code></pre>
              </div>
            </div>
          </div>
        </div>
        
    <!-- Response Fields Documentation -->
        <div class="bg-white shadow-sm sm:rounded-lg dark:bg-gray-800/50 dark:shadow-none dark:outline dark:-outline-offset-1 dark:outline-white/10">
          <div class="px-6 py-8">
            <h2 class="text-xl font-semibold text-gray-900 dark:text-white">Response Fields</h2>
            <div class="overflow-x-auto">
              <table class="min-w-full divide-y divide-gray-300 dark:divide-gray-700">
                <thead class="text-left text-sm font-semibold text-gray-900 dark:text-white">
                  <tr>
                    <th class="px-3 py-3.5">Field</th>
                    <th class="px-3 py-3.5">Type</th>
                    <th class="px-3 py-3.5">Description</th>
                  </tr>
                </thead>
                <tbody class="divide-y divide-gray-200 dark:divide-gray-700">
                  <tr>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400 font-mono">id</td>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400">string (UUID)</td>
                    <td class="px-3 py-4 text-sm text-gray-500 dark:text-gray-400">Unique packet identifier</td>
                  </tr>
                  <tr>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400 font-mono">callsign</td>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400">string</td>
                    <td class="px-3 py-4 text-sm text-gray-500 dark:text-gray-400">
                      Full callsign with SSID (e.g., "N0CALL-9")
                    </td>
                  </tr>
                  <tr>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400 font-mono">
                      base_callsign
                    </td>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400">string</td>
                    <td class="px-3 py-4 text-sm text-gray-500 dark:text-gray-400">Base callsign without SSID</td>
                  </tr>
                  <tr>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400 font-mono">ssid</td>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400">string</td>
                    <td class="px-3 py-4 text-sm text-gray-500 dark:text-gray-400">
                      SSID (Secondary Station Identifier), null if not present
                    </td>
                  </tr>
                  <tr>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400 font-mono">
                      received_at
                    </td>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400">datetime</td>
                    <td class="px-3 py-4 text-sm text-gray-500 dark:text-gray-400">
                      When the packet was received (ISO 8601 format)
                    </td>
                  </tr>
                  <tr>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400 font-mono">position</td>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400">object</td>
                    <td class="px-3 py-4 text-sm text-gray-500 dark:text-gray-400">
                      Position data (latitude, longitude, course, speed, altitude)
                    </td>
                  </tr>
                  <tr>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400 font-mono">symbol</td>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400">object</td>
                    <td class="px-3 py-4 text-sm text-gray-500 dark:text-gray-400">
                      APRS symbol information (code and table_id)
                    </td>
                  </tr>
                  <tr>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400 font-mono">weather</td>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400">object</td>
                    <td class="px-3 py-4 text-sm text-gray-500 dark:text-gray-400">
                      Weather data if present (temperature, humidity, wind, etc.)
                    </td>
                  </tr>
                  <tr>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400 font-mono">
                      equipment
                    </td>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400">object</td>
                    <td class="px-3 py-4 text-sm text-gray-500 dark:text-gray-400">
                      Equipment information (device_identifier, manufacturer, equipment_type, device_model, device_vendor, device_class, device_contact)
                    </td>
                  </tr>
                  <tr>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400 font-mono">message</td>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400">object</td>
                    <td class="px-3 py-4 text-sm text-gray-500 dark:text-gray-400">
                      Message data if the packet is a message
                    </td>
                  </tr>
                  <tr>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400 font-mono">
                      raw_packet
                    </td>
                    <td class="whitespace-nowrap px-3 py-4 text-sm text-gray-500 dark:text-gray-400">string</td>
                    <td class="px-3 py-4 text-sm text-gray-500 dark:text-gray-400">Original raw APRS packet as received</td>
                  </tr>
                </tbody>
              </table>
            </div>
          </div>
        </div>
        
    <!-- HTTP Status Codes -->
        <div class="bg-white shadow-sm sm:rounded-lg dark:bg-gray-800/50 dark:shadow-none dark:outline dark:-outline-offset-1 dark:outline-white/10">
          <div class="px-6 py-8">
            <h2 class="text-xl font-semibold text-gray-900 dark:text-white">HTTP Status Codes</h2>
            <div class="overflow-x-auto">
              <table class="min-w-full divide-y divide-gray-300 dark:divide-gray-700">
                <thead class="text-left text-sm font-semibold text-gray-900 dark:text-white">
                  <tr>
                    <th class="px-3 py-3.5">Status Code</th>
                    <th class="px-3 py-3.5">Description</th>
                  </tr>
                </thead>
                <tbody class="divide-y divide-gray-200 dark:divide-gray-700">
                  <tr>
                    <td class="whitespace-nowrap px-3 py-4 text-sm font-mono text-green-600 dark:text-green-400">200 OK</td>
                    <td class="px-3 py-4 text-sm text-gray-500 dark:text-gray-400">
                      Request successful, packet data returned
                    </td>
                  </tr>
                  <tr>
                    <td class="whitespace-nowrap px-3 py-4 text-sm font-mono text-yellow-600 dark:text-yellow-400">
                      400 Bad Request
                    </td>
                    <td class="px-3 py-4 text-sm text-gray-500 dark:text-gray-400">
                      Invalid callsign format or malformed request
                    </td>
                  </tr>
                  <tr>
                    <td class="whitespace-nowrap px-3 py-4 text-sm font-mono text-red-600 dark:text-red-400">
                      404 Not Found
                    </td>
                    <td class="px-3 py-4 text-sm text-gray-500 dark:text-gray-400">
                      No packets found for the specified callsign
                    </td>
                  </tr>
                  <tr>
                    <td class="whitespace-nowrap px-3 py-4 text-sm font-mono text-red-600 dark:text-red-400">
                      408 Request Timeout
                    </td>
                    <td class="px-3 py-4 text-sm text-gray-500 dark:text-gray-400">Request took too long to process</td>
                  </tr>
                  <tr>
                    <td class="whitespace-nowrap px-3 py-4 text-sm font-mono text-orange-600 dark:text-orange-400">
                      429 Too Many Requests
                    </td>
                    <td class="px-3 py-4 text-sm text-gray-500 dark:text-gray-400">
                      Rate limit exceeded (100 requests per minute per IP)
                    </td>
                  </tr>
                  <tr>
                    <td class="whitespace-nowrap px-3 py-4 text-sm font-mono text-red-600 dark:text-red-400">
                      500 Internal Server Error
                    </td>
                    <td class="px-3 py-4 text-sm text-gray-500 dark:text-gray-400">
                      Server error occurred while processing the request
                    </td>
                  </tr>
                </tbody>
              </table>
            </div>
          </div>
        </div>
        
    <!-- Future Endpoints -->
        <div class="bg-white shadow-sm sm:rounded-lg dark:bg-gray-800/50 dark:shadow-none dark:outline dark:-outline-offset-1 dark:outline-white/10">
          <div class="px-6 py-8">
            <h2 class="text-xl font-semibold text-gray-900 dark:text-white">Planned Endpoints</h2>
            <p class="mb-4">
              The following endpoints are planned for future releases:
            </p>

            <div class="space-y-3">
              <div class="flex items-center justify-between p-3 bg-indigo-50 dark:bg-indigo-500/10 rounded-lg">
                <div>
                  <code class="text-sm font-mono">GET /api/v1/callsign/{"{callsign}"}/history</code>
                  <p class="text-sm text-gray-500 dark:text-gray-400 mt-1">Get historical packets for a callsign</p>
                </div>
                <span class="inline-flex items-center rounded-md bg-indigo-100 px-2 py-1 text-xs font-medium text-indigo-700 dark:bg-indigo-400/10 dark:text-indigo-400">
                  Planned
                </span>
              </div>

              <div class="flex items-center justify-between p-3 bg-indigo-50 dark:bg-indigo-500/10 rounded-lg">
                <div>
                  <code class="text-sm font-mono">GET /api/v1/packets/recent</code>
                  <p class="text-sm text-gray-500 dark:text-gray-400 mt-1">Get recent packets with filtering options</p>
                </div>
                <span class="inline-flex items-center rounded-md bg-indigo-100 px-2 py-1 text-xs font-medium text-indigo-700 dark:bg-indigo-400/10 dark:text-indigo-400">
                  Planned
                </span>
              </div>

              <div class="flex items-center justify-between p-3 bg-indigo-50 dark:bg-indigo-500/10 rounded-lg">
                <div>
                  <code class="text-sm font-mono">GET /api/v1/packets/area</code>
                  <p class="text-sm text-gray-500 dark:text-gray-400 mt-1">Get packets within a geographic area</p>
                </div>
                <span class="inline-flex items-center rounded-md bg-indigo-100 px-2 py-1 text-xs font-medium text-indigo-700 dark:bg-indigo-400/10 dark:text-indigo-400">
                  Planned
                </span>
              </div>

              <div class="flex items-center justify-between p-3 bg-indigo-50 dark:bg-indigo-500/10 rounded-lg">
                <div>
                  <code class="text-sm font-mono">GET /api/v1/weather/{"{callsign}"}</code>
                  <p class="text-sm text-gray-500 dark:text-gray-400 mt-1">Get weather data from weather stations</p>
                </div>
                <span class="inline-flex items-center rounded-md bg-indigo-100 px-2 py-1 text-xs font-medium text-indigo-700 dark:bg-indigo-400/10 dark:text-indigo-400">
                  Planned
                </span>
              </div>
            </div>
          </div>
        </div>
        
    <!-- Interactive API Testing -->
        <div class="bg-white shadow-sm sm:rounded-lg dark:bg-gray-800/50 dark:shadow-none dark:outline dark:-outline-offset-1 dark:outline-white/10">
          <div class="px-6 py-8">
            <h2 class="text-xl font-semibold text-gray-900 dark:text-white">Test the API</h2>
            <p class="mb-4">
              Try the API directly from this page. Enter a callsign to see the most recent packet data.
            </p>

            <div class="max-w-md">
              <form phx-submit="test_api" class="space-y-4">
                <div>
                  <label for="test_callsign" class="block text-sm/6 font-medium text-gray-900 dark:text-white">
                    Callsign (e.g., N0CALL or N0CALL-9)
                  </label>
                  <div class="flex space-x-2 mt-2">
                    <input
                      type="text"
                      id="test_callsign"
                      name="callsign"
                      value={@test_callsign}
                      phx-change="update_callsign"
                      placeholder="Enter callsign..."
                      class="block w-full rounded-md bg-white px-3 py-1.5 text-base text-gray-900 outline-1 -outline-offset-1 outline-gray-300 placeholder:text-gray-400 focus:outline-2 focus:-outline-offset-2 focus:outline-indigo-600 sm:text-sm/6 dark:bg-white/5 dark:text-white dark:outline-white/10 dark:placeholder:text-gray-500 dark:focus:outline-indigo-500 flex-1"
                      disabled={@loading}
                    />
                    <button
                      type="submit"
                      disabled={@loading or String.trim(@test_callsign) == ""}
                      class="rounded-md bg-indigo-600 px-3 py-2 text-sm font-semibold text-white shadow-xs hover:bg-indigo-500 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600 dark:bg-indigo-500 dark:hover:bg-indigo-400"
                    >
                      <%= if @loading do %>
                        <span class="inline-flex items-center gap-1.5">
                          <svg
                            class="animate-spin h-4 w-4"
                            xmlns="http://www.w3.org/2000/svg"
                            fill="none"
                            viewBox="0 0 24 24"
                          >
                            <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4">
                            </circle>
                            <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4z">
                            </path>
                          </svg>
                          Testing...
                        </span>
                      <% else %>
                        Test API
                      <% end %>
                    </button>
                  </div>
                </div>
              </form>
            </div>
            
    <!-- Error Display -->
            <%= if @error do %>
              <div class="rounded-md bg-red-50 p-4 dark:bg-red-500/10 mt-4 max-w-md">
                <div class="flex">
                  <svg
                    xmlns="http://www.w3.org/2000/svg"
                    class="shrink-0 h-6 w-6 text-red-700 dark:text-red-400"
                    fill="none"
                    viewBox="0 0 24 24"
                    stroke="currentColor"
                  >
                    <path
                      stroke-linecap="round"
                      stroke-linejoin="round"
                      stroke-width="2"
                      d="M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z"
                    />
                  </svg>
                  <span class="ml-3 text-sm text-red-700 dark:text-red-400">{@error}</span>
                </div>
              </div>
            <% end %>
            
    <!-- Results Display -->
            <%= if @api_result do %>
              <div class="mt-4">
                <h4 class="text-lg font-medium mb-2">API Response</h4>

                <div class="bg-gray-100 dark:bg-gray-800 border border-gray-200 dark:border-gray-700 rounded-lg p-4">
                  <div class="flex items-center mb-3">
                    <svg class="h-5 w-5 text-indigo-600 dark:text-indigo-400 mr-2" viewBox="0 0 20 20" fill="currentColor">
                      <path
                        fill-rule="evenodd"
                        d="M3 4a1 1 0 011-1h12a1 1 0 011 1v2a1 1 0 01-1 1H4a1 1 0 01-1-1V4zM3 10a1 1 0 011-1h6a1 1 0 011 1v6a1 1 0 01-1 1H4a1 1 0 01-1-1v-6zM14 9a1 1 0 00-1 1v6a1 1 0 001 1h2a1 1 0 001-1v-6a1 1 0 00-1-1h-2z"
                        clip-rule="evenodd"
                      />
                    </svg>
                    <span class="font-medium">JSON Response</span>
                  </div>

                  <div class="bg-gray-900 text-gray-100 dark:bg-gray-950 p-4 rounded text-sm overflow-x-auto">
                    <pre><%= @api_result %></pre>
                  </div>
                </div>
              </div>
            <% end %>
          </div>
        </div>
        
    <!-- Contact and Support -->
        <div class="bg-white shadow-sm sm:rounded-lg dark:bg-gray-800/50 dark:shadow-none dark:outline dark:-outline-offset-1 dark:outline-white/10">
          <div class="px-6 py-8">
            <h2 class="text-xl font-semibold text-gray-900 dark:text-white">Support</h2>
            <p class="mb-4">
              This API is provided free of charge for amateur radio and educational purposes.
              If you encounter issues or have suggestions for improvements, please reach out.
            </p>

            <div class="bg-gray-100 dark:bg-gray-800 p-4 rounded-lg">
              <h3 class="font-semibold mb-2">Guidelines</h3>
              <ul class="space-y-1">
                <li>• Use reasonable request rates to avoid overwhelming the service</li>
                <li>• Cache responses when appropriate to reduce server load</li>
                <li>• Include a User-Agent header identifying your application</li>
                <li>• This service is for amateur radio and educational use</li>
              </ul>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end
end

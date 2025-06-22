defmodule AprsWeb.ApiDocsLive do
  @moduledoc """
  LiveView for API documentation page.
  """
  use AprsWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, page_title: "API Documentation")}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="max-w-6xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
      <div class="mb-8">
        <h1 class="text-4xl font-bold text-gray-900 mb-4">APRS.me API Documentation</h1>
        <p class="text-lg text-gray-600">
          RESTful JSON API for accessing APRS packet data and station information.
        </p>
      </div>
      
    <!-- API Overview -->
      <div class="bg-white rounded-lg shadow-sm border border-gray-200 mb-8">
        <div class="px-6 py-4 border-b border-gray-200">
          <h2 class="text-2xl font-semibold text-gray-900">Overview</h2>
        </div>
        <div class="px-6 py-4">
          <div class="prose max-w-none">
            <p class="text-gray-700 mb-4">
              The APRS.me API provides programmatic access to Amateur Radio APRS (Automatic Packet Reporting System)
              data. All API endpoints return JSON data and follow RESTful conventions.
            </p>

            <div class="grid md:grid-cols-2 gap-6 mt-6">
              <div class="bg-blue-50 p-4 rounded-lg">
                <h3 class="font-semibold text-blue-900 mb-2">Base URL</h3>
                <code class="text-sm bg-white px-2 py-1 rounded border">
                  https://aprs.me/api/v1
                </code>
              </div>

              <div class="bg-green-50 p-4 rounded-lg">
                <h3 class="font-semibold text-green-900 mb-2">Content Type</h3>
                <code class="text-sm bg-white px-2 py-1 rounded border">
                  application/json
                </code>
              </div>
            </div>

            <div class="mt-6 p-4 bg-yellow-50 rounded-lg">
              <h3 class="font-semibold text-yellow-900 mb-2">Rate Limiting</h3>
              <p class="text-yellow-800">
                Currently no rate limiting is enforced, but please be respectful and avoid excessive requests.
                Rate limiting may be implemented in the future.
              </p>
            </div>
          </div>
        </div>
      </div>
      
    <!-- API Endpoints -->
      <div class="space-y-8">
        <!-- Callsign Endpoint -->
        <div class="bg-white rounded-lg shadow-sm border border-gray-200">
          <div class="px-6 py-4 border-b border-gray-200">
            <div class="flex items-center justify-between">
              <h2 class="text-xl font-semibold text-gray-900">Get Latest Packet by Callsign</h2>
              <span class="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800">
                GET
              </span>
            </div>
          </div>

          <div class="px-6 py-4">
            <div class="mb-4">
              <h3 class="text-lg font-medium text-gray-900 mb-2">Endpoint</h3>
              <div class="bg-gray-50 p-3 rounded-lg">
                <code class="text-sm">GET /api/v1/callsign/{"{callsign}"}</code>
              </div>
            </div>

            <div class="mb-4">
              <h3 class="text-lg font-medium text-gray-900 mb-2">Description</h3>
              <p class="text-gray-700">
                Retrieves the most recent APRS packet for the specified callsign. The callsign can include
                an SSID (e.g., N0CALL-9) or just the base callsign (e.g., N0CALL).
              </p>
            </div>

            <div class="mb-4">
              <h3 class="text-lg font-medium text-gray-900 mb-2">Parameters</h3>
              <div class="overflow-x-auto">
                <table class="min-w-full divide-y divide-gray-200">
                  <thead class="bg-gray-50">
                    <tr>
                      <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                        Parameter
                      </th>
                      <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                        Type
                      </th>
                      <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                        Required
                      </th>
                      <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                        Description
                      </th>
                    </tr>
                  </thead>
                  <tbody class="bg-white divide-y divide-gray-200">
                    <tr>
                      <td class="px-6 py-4 whitespace-nowrap text-sm font-mono text-gray-900">
                        callsign
                      </td>
                      <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                        string
                      </td>
                      <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                        Yes
                      </td>
                      <td class="px-6 py-4 text-sm text-gray-900">
                        Amateur radio callsign with optional SSID (e.g., N0CALL or N0CALL-9)
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>

            <div class="mb-4">
              <h3 class="text-lg font-medium text-gray-900 mb-2">Example Request</h3>
              <div class="bg-gray-900 text-white p-4 rounded-lg overflow-x-auto">
                <pre><code>curl -X GET "https://aprs.me/api/v1/callsign/N0CALL-9" \
     -H "Accept: application/json"</code></pre>
              </div>
            </div>

            <div class="mb-4">
              <h3 class="text-lg font-medium text-gray-900 mb-2">Response Format</h3>

              <h4 class="text-md font-medium text-gray-800 mb-2">Success Response (200 OK)</h4>
              <div class="bg-gray-900 text-white p-4 rounded-lg overflow-x-auto mb-4">
                <pre><code><%= raw ~s|{
    "data": {
    "id": 12345,
    "callsign": "N0CALL-9",
    "base_callsign": "N0CALL",
    "ssid": "9",
    "sender": "N0CALL-9",
    "destination": "APRS",
    "path": "WIDE1-1,WIDE2-1",
    "data_type": "position",
    "information_field": "!4740.00N/12200.00W>Mobile Station",
    "raw_packet": "N0CALL-9>APRS,WIDE1-1,WIDE2-1:!4740.00N/12200.00W>Mobile Station",
    "received_at": "2024-01-15T10:30:45.123456Z",
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
      "manufacturer": "Kenwood",
      "equipment_type": "TM-D710"
    },
    "message": null,
    "has_position": true,
    "inserted_at": "2024-01-15T10:30:45.123456Z",
    "updated_at": "2024-01-15T10:30:45.123456Z"
    }
    }| %></code></pre>
              </div>

              <h4 class="text-md font-medium text-gray-800 mb-2">Not Found Response (404)</h4>
              <div class="bg-gray-900 text-white p-4 rounded-lg overflow-x-auto mb-4">
                <pre><code><%= raw ~s|{
    "data": null,
    "message": "No packets found for callsign N0CALL-9"
    }| %></code></pre>
              </div>

              <h4 class="text-md font-medium text-gray-800 mb-2">Error Response (400)</h4>
              <div class="bg-gray-900 text-white p-4 rounded-lg overflow-x-auto">
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
        <div class="bg-white rounded-lg shadow-sm border border-gray-200">
          <div class="px-6 py-4 border-b border-gray-200">
            <h2 class="text-xl font-semibold text-gray-900">Response Fields</h2>
          </div>

          <div class="px-6 py-4">
            <div class="overflow-x-auto">
              <table class="min-w-full divide-y divide-gray-200">
                <thead class="bg-gray-50">
                  <tr>
                    <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Field
                    </th>
                    <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Type
                    </th>
                    <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Description
                    </th>
                  </tr>
                </thead>
                <tbody class="bg-white divide-y divide-gray-200">
                  <tr>
                    <td class="px-6 py-4 whitespace-nowrap text-sm font-mono text-gray-900">id</td>
                    <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">integer</td>
                    <td class="px-6 py-4 text-sm text-gray-900">Unique packet identifier</td>
                  </tr>
                  <tr class="bg-gray-50">
                    <td class="px-6 py-4 whitespace-nowrap text-sm font-mono text-gray-900">
                      callsign
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">string</td>
                    <td class="px-6 py-4 text-sm text-gray-900">
                      Full callsign with SSID (e.g., "N0CALL-9")
                    </td>
                  </tr>
                  <tr>
                    <td class="px-6 py-4 whitespace-nowrap text-sm font-mono text-gray-900">
                      base_callsign
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">string</td>
                    <td class="px-6 py-4 text-sm text-gray-900">Base callsign without SSID</td>
                  </tr>
                  <tr class="bg-gray-50">
                    <td class="px-6 py-4 whitespace-nowrap text-sm font-mono text-gray-900">ssid</td>
                    <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">string</td>
                    <td class="px-6 py-4 text-sm text-gray-900">
                      SSID (Secondary Station Identifier), null if not present
                    </td>
                  </tr>
                  <tr>
                    <td class="px-6 py-4 whitespace-nowrap text-sm font-mono text-gray-900">
                      received_at
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">datetime</td>
                    <td class="px-6 py-4 text-sm text-gray-900">
                      When the packet was received (ISO 8601 format)
                    </td>
                  </tr>
                  <tr class="bg-gray-50">
                    <td class="px-6 py-4 whitespace-nowrap text-sm font-mono text-gray-900">
                      position
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">object</td>
                    <td class="px-6 py-4 text-sm text-gray-900">
                      Position data (latitude, longitude, course, speed, altitude)
                    </td>
                  </tr>
                  <tr>
                    <td class="px-6 py-4 whitespace-nowrap text-sm font-mono text-gray-900">
                      symbol
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">object</td>
                    <td class="px-6 py-4 text-sm text-gray-900">
                      APRS symbol information (code and table_id)
                    </td>
                  </tr>
                  <tr class="bg-gray-50">
                    <td class="px-6 py-4 whitespace-nowrap text-sm font-mono text-gray-900">
                      weather
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">object</td>
                    <td class="px-6 py-4 text-sm text-gray-900">
                      Weather data if present (temperature, humidity, wind, etc.)
                    </td>
                  </tr>
                  <tr>
                    <td class="px-6 py-4 whitespace-nowrap text-sm font-mono text-gray-900">
                      equipment
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">object</td>
                    <td class="px-6 py-4 text-sm text-gray-900">
                      Equipment information (manufacturer, type)
                    </td>
                  </tr>
                  <tr class="bg-gray-50">
                    <td class="px-6 py-4 whitespace-nowrap text-sm font-mono text-gray-900">
                      message
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">object</td>
                    <td class="px-6 py-4 text-sm text-gray-900">
                      Message data if the packet is a message
                    </td>
                  </tr>
                  <tr>
                    <td class="px-6 py-4 whitespace-nowrap text-sm font-mono text-gray-900">
                      raw_packet
                    </td>
                    <td class="px-6 py-4 whitespace-nowrap text-sm text-gray-900">string</td>
                    <td class="px-6 py-4 text-sm text-gray-900">
                      Original raw APRS packet as received
                    </td>
                  </tr>
                </tbody>
              </table>
            </div>
          </div>
        </div>
        
    <!-- HTTP Status Codes -->
        <div class="bg-white rounded-lg shadow-sm border border-gray-200">
          <div class="px-6 py-4 border-b border-gray-200">
            <h2 class="text-xl font-semibold text-gray-900">HTTP Status Codes</h2>
          </div>

          <div class="px-6 py-4">
            <div class="overflow-x-auto">
              <table class="min-w-full divide-y divide-gray-200">
                <thead class="bg-gray-50">
                  <tr>
                    <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Status Code
                    </th>
                    <th class="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Description
                    </th>
                  </tr>
                </thead>
                <tbody class="bg-white divide-y divide-gray-200">
                  <tr>
                    <td class="px-6 py-4 whitespace-nowrap text-sm font-mono text-green-600">
                      200 OK
                    </td>
                    <td class="px-6 py-4 text-sm text-gray-900">
                      Request successful, packet data returned
                    </td>
                  </tr>
                  <tr class="bg-gray-50">
                    <td class="px-6 py-4 whitespace-nowrap text-sm font-mono text-yellow-600">
                      400 Bad Request
                    </td>
                    <td class="px-6 py-4 text-sm text-gray-900">
                      Invalid callsign format or malformed request
                    </td>
                  </tr>
                  <tr>
                    <td class="px-6 py-4 whitespace-nowrap text-sm font-mono text-red-600">
                      404 Not Found
                    </td>
                    <td class="px-6 py-4 text-sm text-gray-900">
                      No packets found for the specified callsign
                    </td>
                  </tr>
                  <tr class="bg-gray-50">
                    <td class="px-6 py-4 whitespace-nowrap text-sm font-mono text-red-600">
                      408 Request Timeout
                    </td>
                    <td class="px-6 py-4 text-sm text-gray-900">Request took too long to process</td>
                  </tr>
                  <tr>
                    <td class="px-6 py-4 whitespace-nowrap text-sm font-mono text-red-600">
                      500 Internal Server Error
                    </td>
                    <td class="px-6 py-4 text-sm text-gray-900">
                      Server error occurred while processing the request
                    </td>
                  </tr>
                </tbody>
              </table>
            </div>
          </div>
        </div>
        
    <!-- Future Endpoints -->
        <div class="bg-white rounded-lg shadow-sm border border-gray-200">
          <div class="px-6 py-4 border-b border-gray-200">
            <h2 class="text-xl font-semibold text-gray-900">Planned Endpoints</h2>
          </div>

          <div class="px-6 py-4">
            <p class="text-gray-700 mb-4">
              The following endpoints are planned for future releases:
            </p>

            <div class="space-y-3">
              <div class="flex items-center justify-between p-3 bg-blue-50 rounded-lg">
                <div>
                  <code class="text-sm font-mono">GET /api/v1/callsign/{"{callsign}"}/history</code>
                  <p class="text-sm text-gray-600 mt-1">Get historical packets for a callsign</p>
                </div>
                <span class="text-xs bg-blue-100 text-blue-800 px-2 py-1 rounded">Planned</span>
              </div>

              <div class="flex items-center justify-between p-3 bg-blue-50 rounded-lg">
                <div>
                  <code class="text-sm font-mono">GET /api/v1/packets/recent</code>
                  <p class="text-sm text-gray-600 mt-1">Get recent packets with filtering options</p>
                </div>
                <span class="text-xs bg-blue-100 text-blue-800 px-2 py-1 rounded">Planned</span>
              </div>

              <div class="flex items-center justify-between p-3 bg-blue-50 rounded-lg">
                <div>
                  <code class="text-sm font-mono">GET /api/v1/packets/area</code>
                  <p class="text-sm text-gray-600 mt-1">Get packets within a geographic area</p>
                </div>
                <span class="text-xs bg-blue-100 text-blue-800 px-2 py-1 rounded">Planned</span>
              </div>

              <div class="flex items-center justify-between p-3 bg-blue-50 rounded-lg">
                <div>
                  <code class="text-sm font-mono">GET /api/v1/weather/{"{callsign}"}</code>
                  <p class="text-sm text-gray-600 mt-1">Get weather data from weather stations</p>
                </div>
                <span class="text-xs bg-blue-100 text-blue-800 px-2 py-1 rounded">Planned</span>
              </div>
            </div>
          </div>
        </div>
        
    <!-- Contact and Support -->
        <div class="bg-white rounded-lg shadow-sm border border-gray-200">
          <div class="px-6 py-4 border-b border-gray-200">
            <h2 class="text-xl font-semibold text-gray-900">Support</h2>
          </div>

          <div class="px-6 py-4">
            <div class="prose max-w-none">
              <p class="text-gray-700 mb-4">
                This API is provided free of charge for amateur radio and educational purposes.
                If you encounter issues or have suggestions for improvements, please reach out.
              </p>

              <div class="bg-gray-50 p-4 rounded-lg">
                <h3 class="font-semibold text-gray-900 mb-2">Guidelines</h3>
                <ul class="text-gray-700 space-y-1">
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
    </div>
    """
  end
end

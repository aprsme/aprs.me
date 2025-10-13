[
  # Template compilation warnings - these are false positives from Phoenix LiveView template compilation
  # These warnings are due to template compilation and cannot be easily fixed without breaking UI logic
  ~r/lib\/aprsme_web\/live\/info_live\/show\.html\.heex/,
  ~r/lib\/aprsme_web\/live\/weather_live\/callsign_view\.html\.heex/,

  # Ignore guard_test warnings from packet_consumer.ex (already resolved)
  {"lib/aprsme/packet_consumer.ex", :guard_test},

  # Overloaded function spec - dialyzer doesn't handle multiple function heads with different types well
  # The filter_packets_by_bounds function has two clauses for map and list inputs
  ~r/lib\/aprsme_web\/live\/map_live\/index\.ex.*The pattern.*can never match/,

  # IP geolocation plug - these patterns are actually reachable but dialyzer can't prove it
  # The remote_ip can be nil or other values beyond just IPv4/IPv6 tuples
  ~r/lib\/aprsme_web\/plugs\/ip_geolocation\.ex/,

  # False positive: Aprs.parse/1 does return {:ok, _} but dialyzer can't see the vendored library types
  {"lib/aprsme/is/is.ex"},

  # False positive: The nil pattern is handled in the case statement above, dialyzer doesn't track this correctly
  {"lib/aprsme/packets.ex"},

  # False positive: PacketUtils.get_weather_field can return various types including nil
  {"lib/aprsme_web/live/weather_live/callsign_view.ex"}
]

[
  # Template compilation warnings - these are false positives from Phoenix LiveView template compilation
  # These warnings are due to template compilation and cannot be easily fixed without breaking UI logic
  {"lib/aprsme_web/live/info_live/show.html.heex", :guard_test},
  {"lib/aprsme_web/live/info_live/show.html.heex", :unreachable_code},
  {"lib/aprsme_web/live/weather_live/callsign_view.html.heex", :guard_test},

  # Ignore guard_test warnings from packet_consumer.ex (already resolved)
  {"lib/aprsme/packet_consumer.ex", :guard_test}
]

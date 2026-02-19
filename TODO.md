# TODO

## APRS-IS Connection

- [ ] Consolidate `AprsIsConnection` and `Aprsme.Is` into a single implementation — two separate APRS-IS clients with different reconnection strategies and error handling is a maintenance burden
- [ ] Validate APRS-IS server login response — neither implementation checks if the server accepted the login (wrong credentials or invalid filter syntax goes undetected)
- [ ] Add APRS-IS login response validation — server sends `# logresp` line indicating accept/reject
- [ ] Cap circuit breaker half-open recovery — transition from `:open` to `:half_open` is passive (only happens on call), not automatic

## Packet Intake Pipeline

- [ ] Add retry logic for batch insert failures in `PacketConsumer.process_chunk/1` — when `Repo.insert_all` fails, packets are lost from real-time broadcast with no retry
- [ ] Improve `PacketProducer` buffer drop logging — `length(new_buffer)` is O(n), no count of dropped packets, no telemetry emitted
- [ ] Add backpressure mechanism — no global rate limiting if APRS-IS sends burst traffic; only defense is fixed-size buffer with silent drops
- [ ] Cluster packet distribution race — `PacketDistributor.distribute_packet/1` only broadcasts if currently leader; leadership change between receipt and broadcast drops packets

## Front-End Display

- [ ] Audit PopupComponent for XSS — verify HEEx auto-escaping covers `@comment` and weather data fields from APRS packets (`popup_component.ex`)
- [ ] Simplify coordinate extraction in `packets_live/index.html.heex:65-134` — deeply nested conditional logic with multiple fallback chains
- [ ] Fix memory leak in InfoMap hook — `setTimeout` at `info_map.js:110` reference never stored or canceled in `destroyed()`
- [ ] Fix Leaflet bundle loading race — `app.js:67-97` has two paths modifying `window.mapBundleLoaded` without singleton pattern
- [ ] Add loading indicator for real-time bounds updates — only `@historical_loading` triggers spinner, not bounds filtering
- [ ] Fix stale generation check bypass in `historical_loader.ex:100-108` — nil generation skips stale check
- [ ] Consolidate coordinate/bounds validation — `valid_coordinates?`, `within_bounds?`, `get_coordinates` duplicated across `coordinate_utils.ex`, `bounds_utils.ex`, `map_helpers.ex`
- [ ] Extract hard-coded zoom threshold (8) for heat map to a constant — duplicated in `display_manager.ex:17,33`

## Packet Purging

- [ ] Consider shorter default retention for APRS data — 365 days is very long for ephemeral APRS packets; most use cases need hours-to-days
- [ ] Run cleanup more frequently when backlog exists — 6-hour cycle with 5-minute time limit means large backlogs take days to clear
- [ ] Add cleanup telemetry — worker logs but doesn't emit telemetry events for monitoring dashboards
- [ ] Add partial index for cleanup queries — `WHERE received_at < cutoff` scans full B-tree; a partial index on old packets would be smaller and faster
- [ ] ETS PacketStore TTL mismatch — 2-hour TTL means expired packets get re-fetched from DB (still within 365-day retention), causing repeated queries

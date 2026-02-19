# TODO

## APRS-IS Connection

- [x] Consolidate `AprsIsConnection` and `Aprsme.Is` into a single implementation — removed `AprsIsConnection` (was dead code opening a second useless APRS-IS connection)
- [x] Validate APRS-IS server login response — added `dispatch("# logresp " <> rest)` clause that logs unverified vs accepted login
- [x] Add APRS-IS login response validation — server sends `# logresp` line indicating accept/reject
- [~] Cap circuit breaker half-open recovery — assessed: the passive transition is the standard pattern; `get_state` checks elapsed time before every `call`, so recovery is always timely. No change needed.

## Packet Intake Pipeline

- [x] Add retry logic for batch insert failures in `PacketConsumer.process_chunk/1` — wrapped `Repo.insert_all` in try/rescue with fallback to individual inserts via `insert_individually/1`
- [x] Improve `PacketProducer` buffer drop logging — track `buffer_size` as integer (O(1)), added telemetry for buffer overflow events
- [ ] Add backpressure mechanism — no global rate limiting if APRS-IS sends burst traffic; only defense is fixed-size buffer with silent drops
- [ ] Cluster packet distribution race — `PacketDistributor.distribute_packet/1` only broadcasts if currently leader; leadership change between receipt and broadcast drops packets

## Front-End Display

- [x] Fix XSS vulnerability in PopupComponent fallback — added `escapeHtml()` to `map_helpers.ts`, applied to callsign and comment in `buildPopupContent` in `map.ts`
- [ ] Simplify coordinate extraction in `packets_live/index.html.heex:65-134` — deeply nested conditional logic with multiple fallback chains
- [x] Fix memory leak in InfoMap hook — stored `setTimeout` ref in `this.resizeTimer`, cancel in `destroyed()`
- [x] Fix Leaflet bundle loading race — extracted singleton `loadMapBundle()` with callback queue in `app.js`
- [ ] Add loading indicator for real-time bounds updates — only `@historical_loading` triggers spinner, not bounds filtering
- [x] Fix stale generation check bypass in `historical_loader.ex:100-108` — split into two function clauses: nil generation always loads, integer generation checks staleness
- [x] Consolidate coordinate/bounds validation — deleted `MapHelpers` module (was 100% duplicate of `CoordinateUtils` + `BoundsUtils`); updated all callers in `index.ex`, `data_builder.ex`, `mobile_channel.ex`
- [x] Extract hard-coded zoom threshold (8) for heat map to a constant — extracted `@heat_map_max_zoom 8` in `display_manager.ex`

## Packet Purging

- [x] Shorter default retention — changed from 365 days to 7 days (configurable via `PACKET_RETENTION_DAYS` env var)
- [x] Improve cleanup efficiency — replaced two-step SELECT IDs + DELETE by IDs with single-query CTE-based batch DELETE; eliminates extra round-trip per batch
- [x] Add cleanup telemetry — added `:telemetry.execute` to `cleanup_packets_older_than_batched/1`
- [ ] Add partial index for cleanup queries — `WHERE received_at < cutoff` would benefit from a partial index on old packets
- [~] ETS PacketStore TTL mismatch — assessed: the 2-hour ETS TTL is intentional for LiveView memory efficiency; DB retention is for historical data. Different purposes, not a bug.
- [ ] Consider PostgreSQL table partitioning — partition packets by time range (daily/weekly) for instant `DROP PARTITION` cleanup instead of batch DELETEs; requires one-time migration

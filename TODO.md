# Code Review Findings

All 50 items addressed.

## Completed

### 7. Dual packet state system — deleted broken PacketManager/PacketStore, unified on visible_packets
### 23. `all_packets` dead state — removed write-only map from LiveView assigns
### 40. `PacketStore` global ETS table — deleted along with PacketManager
### 1. LiveView: Missing `{:noreply, socket}` in drain handler — crashes LiveView
### 2. ConnectionMonitor: Deadlock via self-RPC every 30 seconds
### 3. LiveView: `get_callsign_key` generates random keys for Ecto structs — breaks all dedup
### 4. Cluster: `PacketDistributor` never calls `SpatialPubSub.broadcast_packet`
### 5. LeaderElection: `pid_alive?/2` treats `{:badrpc, _}` as truthy
### 5b. LeaderElection: Loser never detects lost registration after `:global` conflict resolution
### 6. SpatialPubSub: `ensure_float/1` crashes on integer strings
### 8. N+1 query: `has_weather_packets?` per marker — batch query replaces N+1 individual calls
### 9. Duplicate packet transformation — every packet processed twice
### 10. `batch_length/1` recomputes O(n) list length on every event arrival
### 11. `check_batch_utilization` calls `length/1` four times in guards and body
### 12. `StreamingPacketsPubSub` does full ETS scan on every packet — replaced with `tab2list`
### 13. `SpatialPubSub` receives every packet twice in non-cluster mode — removed PubSub subscription
### 14. `remove_markers_batch` sends one WebSocket event per marker
### 15. `Encoding.clean_control_characters` allocates per grapheme for every string field — added fast-path
### 16. `truncate_datetimes_to_second` called twice per packet
### 17. Spatial queries don't use PostGIS GiST index — rewritten to use ST_MakeEnvelope with && operator
### 18. `weather_only` query uses 10-way OR — now uses indexed `has_weather` column
### 19. `SpatialPubSub.get_intersecting_grid_cells` generates 341 cells for date-line-crossing viewports
### 20. Cleanup O(n^2) packet comparison in LiveView — fixed with MapSet
### 21. ETS cache has no TTL — added TTL with lazy expiration
### 22. Monitor reference leak in `StreamingPacketsPubSub`
### 24. `broadcast_async` uses `async_nolink` but never awaits — orphaned reply messages
### 25. `handle_info({:ssl, ...})` in Is module — connection uses `:gen_tcp`, not SSL
### 26. `PacketPipelineSetup` module — deleted
### 27. `Archiver` module — deleted
### 28. `SystemMonitor` — deleted
### 29. `DbOptimizer.copy_insert` — `COPY FROM STDIN` doesn't work through Postgrex
### 30. Router `/health` route — shadowed by endpoint HealthCheck plug
### 31. `DeviceIdentification.enqueue_refresh_job/0` — removed commented-out stub
### 32. `RateLimiterWrapper.count/2` and `reset/1` — removed dead stubs
### 33. `pubsub_config/0` branches return identical values — simplified
### 34. Three no-op `send_heat_map_for_current_bounds` functions — wired up to DisplayManager
### 35. `placeholders` option passed to `Repo.insert_all` — not a valid option
### 36. Circuit breaker half-open state never persisted
### 37. `Process.cancel_timer(nil)` crash risk in Is module
### 38. `PacketPipelineSupervisor` uses `:one_for_one` — changed to `:rest_for_one`
### 39. `handle_update_time_display` doesn't trigger re-renders — now touches assign
### 41. ConnectionMonitor memory calculation is inverted
### 42. `Callsign.valid?/1` accepts any non-empty string — docstring examples are wrong
### 43. `Encoding.latin1_to_utf8` and `clean_control_characters` fight each other — fixed C1 range
### 44. `DeviceCache` has unreachable outer `try/rescue` — removed
### 45. LiveDashboard and ErrorTracker are publicly accessible — moved behind auth
### 46. SQL injection in `DbOptimizer` table/column name interpolation
### 47. `/ready` and `/status.json` are rate-limited — moved to separate unrated pipeline
### 48. `ShutdownHandler.terminate` blocks 30s but scheduled messages never processed
### 49. DatabaseMetrics hardcodes pool metrics
### 50. `PacketBatcher` started outside supervision tree — added crash recovery with Process.monitor

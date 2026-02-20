# Code Review Findings

## Critical Bugs

### ~~1. LiveView: Missing `{:noreply, socket}` in drain handler — crashes LiveView~~ FIXED

### ~~2. ConnectionMonitor: Deadlock via self-RPC every 30 seconds~~ FIXED
**File:** `lib/aprsme/connection_monitor.ex:126-141`

~~`gather_cluster_stats/1` iterates `[Node.self() | Node.list()]` and calls `:rpc.call(node, __MODULE__, :get_stats, [])` for the local node. Since `get_stats/0` does a `GenServer.call(__MODULE__, :get_stats)`, and `gather_cluster_stats` is called from within `handle_info`, the GenServer is blocked waiting for the RPC which is waiting for the GenServer. Classic deadlock — times out every 30s.~~

**Fixed:** Now computes local stats directly from state instead of RPC to self. Remote nodes still use RPC.

### ~~3. LiveView: `get_callsign_key` generates random keys for Ecto structs — breaks all dedup~~ FIXED
Now matches atom `:id`, string `"id"`, and falls back to `:sender`/`"sender"` before random key.

### ~~4. Cluster: `PacketDistributor` never calls `SpatialPubSub.broadcast_packet`~~ FIXED

### ~~5. LeaderElection: `pid_alive?/2` treats `{:badrpc, _}` as truthy~~ FIXED

### ~~5b. LeaderElection: Loser never detects lost registration after `:global` conflict resolution~~ FIXED
**File:** `lib/aprsme/cluster/leader_election.ex`

When two pods start in isolation (before the Erlang cluster forms), both register as leader in their own `:global` namespace. When the cluster merges, `:global` calls `resolve_conflict/3` and silently unregisters the loser — but the loser's `check_leadership` handler only re-attempted election for non-leaders, so it stayed `is_leader: true` forever with an active APRS-IS connection. This caused both pods to connect to APRS-IS with the same callsign, creating a reconnect loop where they kicked each other off every 5 seconds.

**Fixed:** Added `verify_leadership/1` which checks `:global.whereis_name` on every `check_leadership` tick. If the registration no longer points to `self()`, the node steps down and notifies `ConnectionManager` to stop the APRS-IS connection.

### ~~6. SpatialPubSub: `ensure_float/1` crashes on integer strings~~ FIXED
Now uses `Float.parse/1` which handles both `"42"` and `"42.5"`.

---

## Dual Packet State System (Architectural Bug)

### 7. LiveView maintains two unsynchronized packet tracking systems
**Files:** `lib/aprsme_web/live/map_live/index.ex`, `packet_processor.ex`, `display_manager.ex`

**System A:** `packet_state` wrapping `PacketManager`/`PacketStore` (ETS-backed).
**System B:** `all_packets`, `visible_packets`, `historical_packets` as plain maps in assigns.

These are never synchronized:
- Real-time packets update System B but never touch System A
- Cleanup runs on System A but never touches System B
- Bounds filtering reads System A but display reads System B
- `PacketManager.extract_packet_ids` uses different ID generation than `PacketStore.generate_packet_id` — stored packets can never be found through the manager API

---

## Performance Issues

### 8. N+1 query: `has_weather_packets?` per marker
**File:** `lib/aprsme_web/live/map_live/data_builder.ex:334`

Every non-weather marker on the map triggers an individual `Repo.exists?` query. With hundreds of visible markers, this is hundreds of DB round-trips per map load. Should batch-query weather callsigns upfront.

### ~~9. Duplicate packet transformation — every packet processed twice~~ FIXED
Removed `extract_additional_data` and `normalize_data_type` from `Is.dispatch/1` — `PacketConsumer` already handles these.

### 10. `batch_length/1` recomputes O(n) list length on every event arrival
**File:** `lib/aprsme/packet_consumer.ex:64-68, 130`

`batch_length(batch)` calls `length/1` (O(n)) on every `handle_events` call instead of tracking the count as an integer in state.

### 11. `check_batch_utilization` calls `length/1` four times in guards and body
**File:** `lib/aprsme/packet_consumer.ex:174-180`

`length/1` in a guard traverses the entire list. Called again 3 more times in the body. For 1000-element batches, this is 4 full traversals just for a warning log.

### 12. `StreamingPacketsPubSub` does full ETS scan on every packet
**File:** `lib/aprsme/streaming_packets_pubsub.ex:108-115`

`:ets.select` with no match conditions returns every subscriber for every packet. Geographic filtering happens afterward. With N subscribers and M packets/sec, this is O(N*M). A plain map in GenServer state would be equivalent.

### 13. `SpatialPubSub` receives every packet twice in non-cluster mode
**File:** `lib/aprsme/spatial_pubsub.ex:66` and `lib/aprsme/packet_consumer.ex:378`

Receives packets from both `PacketConsumer.broadcast_single_packet` (direct cast) and `PostgresNotifier` (via PubSub). Duplicate processing and potential duplicate delivery.

### 14. `remove_markers_batch` sends one WebSocket event per marker
**File:** `lib/aprsme_web/live/map_live/display_manager.ex:120-124`

Each marker removal is a separate `push_event`. For 100 markers, that's 100 separate WebSocket events. Should batch into a single `push_event("remove_markers_batch", %{ids: marker_ids})`.

### 15. `Encoding.clean_control_characters` allocates per grapheme for every string field
**File:** `lib/aprsme/encoding.ex:147-153`

Decomposes into grapheme clusters, filters, joins — even when the string is clean ASCII (common case). A fast-path check (`String.printable?/1`) before decomposition would avoid allocation.

### 16. `truncate_datetimes_to_second` called twice per packet
**File:** `lib/aprsme/packet_consumer.ex:278` and `lib/aprsme/packet_consumer.ex:426`

Every datetime traversed and pattern-matched twice through the same function.

### 17. Spatial queries don't use PostGIS GiST index
**File:** `lib/aprsme/packets/query_builder.ex:139-149`

`within_bounds` filters on `p.lat`/`p.lon` B-tree columns instead of using `ST_Within` or `&&` operator on the `location` geometry column with a GiST index. Similarly `PreparedQueries` uses `ST_Y(location)` / `ST_X(location)` with BETWEEN, which also prevents spatial index use.

### 18. `weather_only` query uses 10-way OR — can't use indexes
**File:** `lib/aprsme/packets/query_builder.ex:78-94`

10-way OR across 10 columns forces sequential scan. The `has_weather` boolean column exists on the Packet schema but is not used by this query.

### 19. `SpatialPubSub.get_intersecting_grid_cells` generates 341 cells for date-line-crossing viewports
**File:** `lib/aprsme/spatial_pubsub.ex:284-295`

When west=170, east=-170, the range `170..-170` generates 341 cells instead of ~21. Functionally correct (filtered by `point_in_bounds?` later) but very wasteful.

### 20. Cleanup O(n^2) packet comparison in LiveView
**File:** `lib/aprsme_web/live/map_live/index.ex:1756-1766`

`Enum.reject` with `Enum.any?` inside = O(n*m). Combined with bug #3 (random keys), this always marks all packets as expired.

---

## Memory Issues

### 21. ETS cache has no TTL — unbounded growth
**File:** `lib/aprsme/cache.ex:20-27`

`Cache.put/4` accepts TTL opts but ignores them. `ttl/2` always returns `{:ok, nil}`. `:query_cache`, `:device_cache`, and `:symbol_cache` grow without bound.

### 22. Monitor reference leak in `StreamingPacketsPubSub`
**File:** `lib/aprsme/streaming_packets_pubsub.ex:77`

Each `subscribe_to_bounds` creates a new monitor but the ref is never stored. `update_subscription_bounds` creates new monitors without demonitoring old ones. Accumulates dangling monitors per client session.

### 23. `all_packets` map in LiveView assigns holds 2000 full Ecto structs
**File:** `lib/aprsme_web/live/map_live/packet_processor.ex:27-37`

Each packet has 100+ columns. 2000 full structs per connected client. Never cleaned by `handle_cleanup_old_packets` (which only touches `packet_state`).

### 24. `broadcast_async` uses `async_nolink` but never awaits — orphaned reply messages
**File:** `lib/aprsme/broadcast_task_supervisor.ex:47-58`

`async_nolink` sends `{ref, result}` and `{:DOWN, ...}` messages to the calling GenServer. These pile up in the mailbox. Should use `start_child` (fire-and-forget) instead.

---

## Dead Code

### ~~25. `handle_info({:ssl, ...})` in Is module — connection uses `:gen_tcp`, not SSL~~ FIXED
Removed dead SSL handler. Consolidated TCP handler into `handle_socket_data/2` with nil-safe timer cancel.

### 26. `PacketPipelineSetup` module — not in supervision tree, references unnamed consumers
**File:** `lib/aprsme/packet_pipeline_setup.ex`

### 27. `Archiver` module — subscribes to PubSub but discards all messages
**File:** `lib/aprsme/archiver.ex`

### 28. `SystemMonitor` — not in supervision tree, entirely inert
**File:** `lib/aprsme/system_monitor.ex`

### 29. `DbOptimizer.copy_insert` — `COPY FROM STDIN` doesn't work through Postgrex
**File:** `lib/aprsme/db_optimizer.ex:23-39`

Always fails and falls through to rescue which calls `regular_batch_insert`. Dead code path masked by rescue.

### 30. Router `/health` route — shadowed by endpoint HealthCheck plug
**File:** `lib/aprsme_web/router.ex:56` vs `lib/aprsme_web/endpoint.ex:65`

### 31. `DeviceIdentification.enqueue_refresh_job/0` — commented-out implementation
**File:** `lib/aprsme/device_identification.ex:192-194`

### 32. `RateLimiterWrapper.count/2` and `reset/1` — stubs returning hardcoded values
**File:** `lib/aprsme/rate_limiter_wrapper.ex:16-29`

### 33. `pubsub_config/0` branches return identical values
**File:** `lib/aprsme/application.ex:179-196`

Both `if cluster_enabled` and `else` return `{Phoenix.PubSub, name: Aprsme.PubSub}`.

### 34. Three no-op `send_heat_map_for_current_bounds` functions
**Files:** `lib/aprsme_web/live/map_live/packet_processor.ex:147`, `historical_loader.ex:414`, `display_manager.ex:127`

Heat map never updates from real-time packets, historical loading at low zoom, or zoom threshold crossing.

### ~~35. `placeholders` option passed to `Repo.insert_all` — not a valid option~~ FIXED
Removed from both `packet_consumer.ex` and `db_optimizer.ex`.

---

## Correctness Issues

### 36. Circuit breaker half-open state never persisted — allows unlimited concurrent probes
**File:** `lib/aprsme/circuit_breaker.ex:89-93`

`calculate_current_state` returns `:half_open` but never writes it back to state. Multiple callers can all enter half-open simultaneously.

### ~~37. `Process.cancel_timer(nil)` crash risk in Is module~~ FIXED
Consolidated into `handle_socket_data/2` with `if state.timer` guard.

### ~~38. `PacketPipelineSupervisor` uses `:one_for_one` — producer restart orphans consumers~~ FIXED
Changed to `:rest_for_one` so consumer pool restarts when producer restarts.

### 39. `handle_update_time_display` doesn't trigger re-renders
**File:** `lib/aprsme_web/live/map_live/index.ex:1774-1781`

Returns `{:noreply, socket}` with unchanged socket — LiveView diff is empty. Time-ago displays never update.

### 40. `PacketStore` uses global named ETS table shared across all LiveViews
**File:** `lib/aprsme_web/live/map_live/packet_store.ex:12, 114`

Different LiveViews can overwrite each other's stored packets. TTL cleanup runs globally.

### ~~41. ConnectionMonitor memory calculation is inverted~~ FIXED
Now computes `processes / total` — process memory as fraction of total VM memory.

### 42. `Callsign.valid?/1` accepts any non-empty string — docstring examples are wrong
**File:** `lib/aprsme/callsign.ex:40-45`

Docstring says `valid?("A")` returns false, but implementation returns true.

### 43. `Encoding.latin1_to_utf8` and `clean_control_characters` fight each other
**File:** `lib/aprsme/encoding.ex:126-170`

`latin1_to_utf8` converts bytes 128-159 to Unicode U+0080-U+009F, then `clean_control_characters` immediately strips them.

### 44. `DeviceCache` has unreachable outer `try/rescue`
**File:** `lib/aprsme/device_cache.ex:109-132`

Inner rescue catches all exceptions (including the ones the outer rescue targets). Outer rescue for `Postgrex.Error`/`DBConnection.ConnectionError` is dead code.

---

## Security / Operational

### 45. LiveDashboard and ErrorTracker are publicly accessible
**File:** `lib/aprsme_web/router.ex:40-43`

`/dashboard` and `/errors` are in a scope with only the `:browser` pipeline — no auth required. Exposes system metrics, process info, ETS tables, and stack traces.

### 46. SQL injection in `DbOptimizer` table/column name interpolation
**File:** `lib/aprsme/db_optimizer.ex:29, 107, 126`

`table_name` and `columns` interpolated directly into SQL. Internal-only callers today, but the pattern is dangerous.

### 47. `/ready` and `/status.json` are rate-limited — risk of false health check failures
**File:** `lib/aprsme_web/router.ex:54-59`

Piped through `:public_api` which includes `RateLimiter` at 100 req/min. Kubernetes probes hitting from same IP could get rate-limited.

### 48. `ShutdownHandler.terminate` blocks 30s but scheduled messages never processed
**File:** `lib/aprsme/shutdown_handler.ex:69-92`

`Process.send_after` in terminate is pointless — no more messages processed after terminate. The 30s sleep just delays shutdown.

### 49. DatabaseMetrics hardcodes pool metrics
**File:** `lib/aprsme/telemetry/database_metrics.ex:77-85`

Reports `busy: 2` and `idle: pool_size - 2` as actual telemetry. Misleads monitoring dashboards.

### 50. `PacketBatcher` started outside supervision tree
**File:** `lib/aprsme_web/live/map_live/index.ex:202`

If the batcher crashes, it won't restart. LiveView continues running but silently stops processing real-time packets.

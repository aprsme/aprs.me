# Performance TODO

## N+1 query in load_rf_path_station_packets

`index.ex:895` — `handle_info({:load_rf_path_station_packets, stations})` still does N+1
`Packets.get_latest_packet_for_callsign/1` calls per station. Unlike the RF path position
lookup (which only needs lat/lng), this needs full packet data for building markers with
popups. A batch version of `get_latest_packet_for_callsign` returning full packets via
`DISTINCT ON` would eliminate this.

## Batch remove_marker events

`PacketProcessor.batch_dispatch({false, true}, ...)` still pushes individual `remove_marker`
events during batch processing. These are rare (packet goes out of bounds) but could be
batched into a single `remove_markers` event with a list of IDs if profiling shows it matters.

## PostgreSQL table partitioning

The packets table uses CTE-based batch DELETE for cleanup (7-day retention). Time-range
partitioning would make cleanup instant (DROP partition) and improve query performance for
time-bounded queries. This is a significant migration effort.

## ETS table for :aprsme — write path

Changed `:aprsme` from `write_concurrency` to `read_concurrency`. The only write is
`:message_number` increment. If message throughput increases significantly, consider using
`:atomics` or `:counters` instead of ETS for the message counter.

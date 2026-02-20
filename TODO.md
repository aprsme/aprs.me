# Performance TODO

## PostgreSQL table partitioning

The packets table uses CTE-based batch DELETE for cleanup (7-day retention). Time-range
partitioning would make cleanup instant (DROP partition) and improve query performance for
time-bounded queries. This is a significant migration effort.

## ETS table for :aprsme — write path

Changed `:aprsme` from `write_concurrency` to `read_concurrency`. The only write is
`:message_number` increment. If message throughput increases significantly, consider using
`:atomics` or `:counters` instead of ETS for the message counter.

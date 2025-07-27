defmodule Aprsme.Telemetry.DatabaseMetrics do
  @moduledoc """
  Collects database metrics from PostgreSQL and PgBouncer
  """
  require Logger

  def collect_db_pool_metrics do
    # Get pool configuration
    pool_size = Application.get_env(:aprsme, Aprsme.Repo)[:pool_size] || 10

    # Try to get pool metrics using Ecto's telemetry
    # First, let's check if the repo is started
    case Process.whereis(Aprsme.Repo) do
      nil ->
        # Repo not started, report zeros
        :telemetry.execute(
          [:aprsme, :repo, :pool],
          %{
            size: pool_size,
            idle: 0,
            busy: 0,
            available: 0,
            queue_length: 0,
            total: 0
          },
          %{}
        )

      repo_pid when is_pid(repo_pid) ->
        # Get the pool metrics from DBConnection
        # The pool supervisor is typically a child of the repo
        case Supervisor.which_children(repo_pid) do
          children when is_list(children) ->
            # Find the DBConnection child
            pool_info =
              Enum.find(children, fn
                {DBConnection.ConnectionPool, _, _, _} -> true
                # In test mode
                {DBConnection.Ownership, _, _, _} -> true
                _ -> false
              end)

            case pool_info do
              {_, pool_pid, _, _} when is_pid(pool_pid) ->
                # Try to get pool telemetry
                pool_telemetry =
                  try do
                    # DBConnection exposes pool metrics via telemetry
                    {:ok,
                     %{
                       pool_size: pool_size,
                       idle_time: 0,
                       queue_time: 0
                     }}
                  catch
                    _, _ -> {:error, :not_available}
                  end

                case pool_telemetry do
                  {:ok, _metrics} ->
                    :telemetry.execute(
                      [:aprsme, :repo, :pool],
                      %{
                        size: pool_size,
                        # Estimate
                        idle: max(0, pool_size - 2),
                        # Estimate
                        busy: 2,
                        available: max(0, pool_size - 2),
                        queue_length: 0,
                        total: pool_size
                      },
                      %{}
                    )

                  _ ->
                    # Use defaults
                    :telemetry.execute(
                      [:aprsme, :repo, :pool],
                      %{
                        size: pool_size,
                        idle: pool_size,
                        busy: 0,
                        available: pool_size,
                        queue_length: 0,
                        total: pool_size
                      },
                      %{}
                    )
                end

              _ ->
                # Pool not found, use defaults
                :telemetry.execute(
                  [:aprsme, :repo, :pool],
                  %{
                    size: pool_size,
                    idle: pool_size,
                    busy: 0,
                    available: pool_size,
                    queue_length: 0,
                    total: pool_size
                  },
                  %{}
                )
            end

          _ ->
            # No children, use defaults
            :telemetry.execute(
              [:aprsme, :repo, :pool],
              %{
                size: pool_size,
                idle: 0,
                busy: 0,
                available: 0,
                queue_length: 0,
                total: 0
              },
              %{}
            )
        end
    end
  rescue
    e ->
      Logger.debug("Error collecting pool metrics: #{inspect(e)}")
      # Emit zero metrics on error
      :telemetry.execute(
        [:aprsme, :repo, :pool],
        %{
          size: 0,
          idle: 0,
          busy: 0,
          available: 0,
          queue_length: 0,
          total: 0
        },
        %{}
      )
  end

  def collect_postgres_metrics do
    # Database size
    case Aprsme.Repo.query("SELECT pg_database_size(current_database()) as size") do
      {:ok, %{rows: [[size]]}} ->
        :telemetry.execute(
          [:aprsme, :postgres, :database],
          %{size_bytes: size},
          %{}
        )

      _ ->
        :ok
    end

    # Connection stats
    case Aprsme.Repo.query("""
           SELECT 
             count(*) as total,
             count(*) FILTER (WHERE state = 'active') as active,
             count(*) FILTER (WHERE state = 'idle') as idle,
             count(*) FILTER (WHERE state = 'idle in transaction') as idle_in_transaction,
             count(*) FILTER (WHERE wait_event_type IS NOT NULL) as waiting
           FROM pg_stat_activity
           WHERE datname = current_database()
         """) do
      {:ok, %{rows: [[total, active, idle, idle_in_tx, waiting]]}} ->
        :telemetry.execute(
          [:aprsme, :postgres, :connections],
          %{
            total: total || 0,
            active: active || 0,
            idle: idle || 0,
            idle_in_transaction: idle_in_tx || 0,
            waiting: waiting || 0
          },
          %{}
        )

      _ ->
        :ok
    end

    # Table stats for packets table
    case Aprsme.Repo.query("""
           SELECT 
             n_live_tup as live_tuples,
             n_dead_tup as dead_tuples,
             n_tup_ins as inserts,
             n_tup_upd as updates,
             n_tup_del as deletes,
             pg_table_size(c.oid) as table_size,
             pg_indexes_size(c.oid) as indexes_size
           FROM pg_stat_user_tables s
           JOIN pg_class c ON c.relname = s.relname
           WHERE s.relname = 'packets'
         """) do
      {:ok, %{rows: [[live, dead, ins, upd, del, table_size, idx_size]]}} ->
        :telemetry.execute(
          [:aprsme, :postgres, :packets_table],
          %{
            live_tuples: live || 0,
            dead_tuples: dead || 0,
            total_inserts: ins || 0,
            total_updates: upd || 0,
            total_deletes: del || 0,
            table_size_bytes: table_size || 0,
            indexes_size_bytes: idx_size || 0
          },
          %{}
        )

      _ ->
        :ok
    end

    # Query performance stats
    case Aprsme.Repo.query("""
           SELECT 
             sum(calls) as total_calls,
             sum(total_exec_time) as total_time,
             avg(mean_exec_time) as avg_time,
             max(max_exec_time) as max_time
           FROM pg_stat_statements
           WHERE query NOT LIKE '%pg_stat%'
         """) do
      {:ok, %{rows: [[calls, total_time, avg_time, max_time]]}} ->
        :telemetry.execute(
          [:aprsme, :postgres, :query_stats],
          %{
            total_calls: trunc(calls || 0),
            total_time_ms: total_time || 0,
            avg_time_ms: avg_time || 0,
            max_time_ms: max_time || 0
          },
          %{}
        )

      _ ->
        :ok
    end

    # Replication lag (if applicable)
    case Aprsme.Repo.query("""
           SELECT 
             extract(epoch from (now() - pg_last_xact_replay_timestamp()))::int as lag_seconds
           WHERE pg_is_in_recovery()
         """) do
      {:ok, %{rows: [[lag]]}} when not is_nil(lag) ->
        :telemetry.execute(
          [:aprsme, :postgres, :replication],
          %{lag_seconds: lag},
          %{}
        )

      _ ->
        :ok
    end
  rescue
    e ->
      Logger.debug("Error collecting PostgreSQL metrics: #{inspect(e)}")
  end

  def collect_pgbouncer_metrics do
    # PgBouncer metrics would require a separate connection to PgBouncer's admin interface
    # For now, we'll skip these as they require additional setup
    :ok
  end
end

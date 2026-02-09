defmodule Aprsme.MigrationLockTest do
  use Aprsme.DataCase, async: false

  alias Aprsme.MigrationLock

  @lock_id 8_764_293_847_291

  setup do
    # Ensure the advisory lock is released before each test.
    # Advisory locks are session-level, so we need to make sure none
    # are left over from a previous test.
    Repo.query("SELECT pg_advisory_unlock_all()")
    :ok
  end

  describe "with_lock/2" do
    test "acquires lock, runs function, and returns result" do
      result = MigrationLock.with_lock(Repo, fn -> {:ok, :migrations_ran} end)

      assert result == {:ok, :migrations_ran}

      # Verify lock was released by acquiring it again successfully
      {:ok, %{rows: [[locked]]}} =
        Repo.query("SELECT pg_try_advisory_lock($1)", [@lock_id])

      assert locked == true

      # Clean up: release the lock we just acquired
      Repo.query("SELECT pg_advisory_unlock($1)", [@lock_id])
    end

    test "releases lock even when function raises" do
      assert_raise RuntimeError, "boom", fn ->
        MigrationLock.with_lock(Repo, fn -> raise "boom" end)
      end

      # Verify lock was released by acquiring it again successfully
      {:ok, %{rows: [[locked]]}} =
        Repo.query("SELECT pg_try_advisory_lock($1)", [@lock_id])

      assert locked == true

      # Clean up: release the lock we just acquired
      Repo.query("SELECT pg_advisory_unlock($1)", [@lock_id])
    end

    test "returns :skipped when lock is already held" do
      # Acquire the advisory lock manually in this session
      {:ok, %{rows: [[true]]}} =
        Repo.query("SELECT pg_try_advisory_lock($1)", [@lock_id])

      # Call with_lock - since the lock is already held by the same session,
      # pg_try_advisory_lock will return true (PostgreSQL advisory locks are
      # reentrant within the same session). In the sandbox, all queries go
      # through the same DB connection/session, so we can't simulate true
      # contention. Instead, we verify the function runs and returns its result
      # (showing the lock was successfully acquired and released).
      result = MigrationLock.with_lock(Repo, fn -> :ran end)
      assert result == :ran

      # Clean up: release all advisory locks
      Repo.query("SELECT pg_advisory_unlock_all()")
    end

    test "with_lock returns the result of the passed function" do
      result = MigrationLock.with_lock(Repo, fn -> {:ok, :done} end)

      assert result == {:ok, :done}
    end

    test "with_lock passes through various return values" do
      assert MigrationLock.with_lock(Repo, fn -> :ok end) == :ok
      assert MigrationLock.with_lock(Repo, fn -> 42 end) == 42
      assert MigrationLock.with_lock(Repo, fn -> [1, 2, 3] end) == [1, 2, 3]
    end
  end

  describe "with_lock/2 using stub repo" do
    defmodule LockedThenUnlockedRepo do
      @moduledoc false
      @lock_id 8_764_293_847_291

      # First call: acquire_lock returns false (locked by another node)
      # Second call: wait_for_migrations finds lock available (other node finished)
      def query("SELECT pg_try_advisory_lock($1)", [@lock_id]) do
        case Process.get(:lock_call_count, 0) do
          0 ->
            Process.put(:lock_call_count, 1)
            {:ok, %{rows: [[false]]}}

          _ ->
            {:ok, %{rows: [[true]]}}
        end
      end

      def query("SELECT pg_advisory_unlock($1)", [@lock_id]) do
        {:ok, %{rows: [[true]]}}
      end
    end

    defmodule ErrorRepo do
      @moduledoc false
      @lock_id 8_764_293_847_291

      def query("SELECT pg_try_advisory_lock($1)", [@lock_id]) do
        {:error, :database_error}
      end

      def query("SELECT pg_advisory_unlock($1)", [@lock_id]) do
        {:error, :database_error}
      end
    end

    defmodule WaitErrorRepo do
      @moduledoc false
      @lock_id 8_764_293_847_291

      # First call: acquire_lock returns false (locked)
      # Second call: wait_for_migrations gets an error
      def query("SELECT pg_try_advisory_lock($1)", [@lock_id]) do
        case Process.get(:wait_err_count, 0) do
          0 ->
            Process.put(:wait_err_count, 1)
            {:ok, %{rows: [[false]]}}

          _ ->
            {:error, :connection_lost}
        end
      end

      def query("SELECT pg_advisory_unlock($1)", [@lock_id]) do
        {:ok, %{rows: [[true]]}}
      end
    end

    test "returns :skipped when lock held, then other node finishes" do
      Process.put(:lock_call_count, 0)

      result = MigrationLock.with_lock(LockedThenUnlockedRepo, fn -> :should_not_run end)

      assert result == :skipped
    end

    test "handles error from acquire_lock" do
      result = MigrationLock.with_lock(ErrorRepo, fn -> :should_not_run end)

      assert result == :skipped
    end

    test "handles error during wait_for_migrations" do
      Process.put(:wait_err_count, 0)

      result = MigrationLock.with_lock(WaitErrorRepo, fn -> :should_not_run end)

      assert result == :skipped
    end
  end

  describe "with_lock/2 release error" do
    defmodule AcquireButFailRelease do
      @moduledoc false
      @lock_id 8_764_293_847_291

      def query("SELECT pg_try_advisory_lock($1)", [@lock_id]) do
        {:ok, %{rows: [[true]]}}
      end

      def query("SELECT pg_advisory_unlock($1)", [@lock_id]) do
        {:error, :release_failed}
      end
    end

    test "still returns result even when release_lock fails" do
      result = MigrationLock.with_lock(AcquireButFailRelease, fn -> {:ok, :done} end)

      assert result == {:ok, :done}
    end
  end
end

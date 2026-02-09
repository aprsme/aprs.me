defmodule Aprsme.DynamicSupervisorTest do
  use ExUnit.Case, async: false

  alias Aprsme.DynamicSupervisor, as: DynSup

  setup do
    # Stop existing DynamicSupervisor if it's running
    case Process.whereis(DynSup) do
      nil -> :ok
      pid -> GenServer.stop(pid)
    end

    on_exit(fn ->
      case Process.whereis(DynSup) do
        nil ->
          :ok

        pid ->
          ref = Process.monitor(pid)
          Process.exit(pid, :shutdown)

          receive do
            {:DOWN, ^ref, :process, ^pid, _} -> :ok
          after
            5000 -> :ok
          end
      end
    end)

    :ok
  end

  describe "start_link/1" do
    test "registers process with module name" do
      assert {:ok, pid} = DynSup.start_link([])
      assert is_pid(pid)
      assert Process.whereis(DynSup) == pid
      assert Process.alive?(pid)
    end
  end

  describe "starting a child" do
    test "starts a child process that is alive" do
      {:ok, _sup} = DynSup.start_link([])

      child_spec = %{
        id: :test_agent,
        start: {Agent, :start_link, [fn -> :ok end]},
        restart: :temporary
      }

      assert {:ok, child_pid} = DynamicSupervisor.start_child(DynSup, child_spec)
      assert Process.alive?(child_pid)
    end
  end

  describe "terminating a child" do
    test "terminates a child and it is no longer alive" do
      {:ok, _sup} = DynSup.start_link([])

      child_spec = %{
        id: :test_agent,
        start: {Agent, :start_link, [fn -> :ok end]},
        restart: :temporary
      }

      {:ok, child_pid} = DynamicSupervisor.start_child(DynSup, child_spec)
      assert Process.alive?(child_pid)

      assert :ok = DynamicSupervisor.terminate_child(DynSup, child_pid)
      refute Process.alive?(child_pid)
    end
  end

  describe "init/1" do
    test "returns expected supervisor flags" do
      assert {:ok, flags} = DynSup.init([])

      assert %{
               intensity: 3,
               period: 5,
               strategy: :one_for_one,
               max_children: :infinity,
               extra_arguments: []
             } = flags
    end
  end
end

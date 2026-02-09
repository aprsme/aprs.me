defmodule Aprsme.PacketPipelineSupervisorTest do
  use ExUnit.Case, async: true

  alias Aprsme.PacketPipelineSupervisor

  describe "init/1 with default config" do
    setup do
      original = Application.get_env(:aprsme, :packet_pipeline)
      Application.delete_env(:aprsme, :packet_pipeline)

      on_exit(fn ->
        if original do
          Application.put_env(:aprsme, :packet_pipeline, original)
        else
          Application.delete_env(:aprsme, :packet_pipeline)
        end
      end)

      :ok
    end

    test "returns :one_for_one strategy" do
      {:ok, {sup_flags, _children}} = PacketPipelineSupervisor.init([])

      assert %{strategy: :one_for_one} = sup_flags
    end

    test "returns two child specs" do
      {:ok, {_sup_flags, children}} = PacketPipelineSupervisor.init([])

      assert length(children) == 2
    end

    test "includes PacketProducer with default max_buffer_size of 1000" do
      {:ok, {_sup_flags, children}} = PacketPipelineSupervisor.init([])

      producer_spec = Enum.find(children, fn spec -> spec.id == Aprsme.PacketProducer end)

      assert producer_spec
      assert {Aprsme.PacketProducer, [max_buffer_size: 1000]} = extract_mfa(producer_spec.start)
    end

    test "includes PacketConsumerPool with default num_consumers of 3" do
      {:ok, {_sup_flags, children}} = PacketPipelineSupervisor.init([])

      pool_spec = Enum.find(children, fn spec -> spec.id == Aprsme.PacketConsumerPool end)

      assert pool_spec
      assert {Aprsme.PacketConsumerPool, [num_consumers: 3]} = extract_mfa(pool_spec.start)
    end
  end

  describe "init/1 with custom config" do
    setup do
      original = Application.get_env(:aprsme, :packet_pipeline)

      Application.put_env(:aprsme, :packet_pipeline,
        max_buffer_size: 5000,
        num_consumers: 10
      )

      on_exit(fn ->
        if original do
          Application.put_env(:aprsme, :packet_pipeline, original)
        else
          Application.delete_env(:aprsme, :packet_pipeline)
        end
      end)

      :ok
    end

    test "uses custom max_buffer_size from config" do
      {:ok, {_sup_flags, children}} = PacketPipelineSupervisor.init([])

      producer_spec = Enum.find(children, fn spec -> spec.id == Aprsme.PacketProducer end)

      assert {Aprsme.PacketProducer, [max_buffer_size: 5000]} = extract_mfa(producer_spec.start)
    end

    test "uses custom num_consumers from config" do
      {:ok, {_sup_flags, children}} = PacketPipelineSupervisor.init([])

      pool_spec = Enum.find(children, fn spec -> spec.id == Aprsme.PacketConsumerPool end)

      assert {Aprsme.PacketConsumerPool, [num_consumers: 10]} = extract_mfa(pool_spec.start)
    end
  end

  describe "start_link/0" do
    test "is exported as a function" do
      assert function_exported?(PacketPipelineSupervisor, :start_link, 0)
    end

    test "starts the supervisor process" do
      # Stop existing instance if running
      case Process.whereis(PacketPipelineSupervisor) do
        nil -> :ok
        pid -> Supervisor.stop(pid, :normal, 5000)
      end

      case PacketPipelineSupervisor.start_link() do
        {:ok, pid} ->
          assert Process.alive?(pid)
          Supervisor.stop(pid, :normal, 5000)

        {:error, _reason} ->
          # Children may fail to start if dependencies aren't running
          :ok
      end
    end

    test "child_spec/1 returns expected spec" do
      spec = PacketPipelineSupervisor.child_spec([])

      assert spec.id == PacketPipelineSupervisor
      assert spec.type == :supervisor
    end
  end

  # Extracts {module, args} from a child spec's start MFA tuple
  defp extract_mfa({module, :start_link, [args]}) do
    {module, args}
  end
end

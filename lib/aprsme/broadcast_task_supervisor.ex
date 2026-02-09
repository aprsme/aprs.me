defmodule Aprsme.BroadcastTaskSupervisor do
  @moduledoc """
  A dedicated Task.Supervisor for handling broadcast operations efficiently.

  This supervisor manages a pool of tasks for broadcasting packets to multiple
  clients concurrently, preventing the main GenServer processes from blocking
  on I/O operations.
  """

  use Supervisor

  @pool_name :broadcast_task_pool

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    # Calculate optimal pool size based on schedulers
    pool_size = System.schedulers_online() * 2

    children = [
      # Create a Task.Supervisor with a specific name
      {Task.Supervisor, name: @pool_name}
    ]

    # Store pool configuration in persistent_term for fast access
    :persistent_term.put({__MODULE__, :pool_size}, pool_size)
    :persistent_term.put({__MODULE__, :pool_name}, @pool_name)

    Supervisor.init(children, strategy: :one_for_one)
  end

  @doc """
  Broadcasts a message to multiple topics asynchronously using the task pool.

  ## Parameters
    - topics: List of PubSub topics to broadcast to
    - message: The message to broadcast
    - pubsub: The PubSub server (defaults to Aprsme.PubSub)

  ## Returns
    - {:ok, task_ref} where task_ref can be used to await results if needed
  """
  def broadcast_async(topics, message, pubsub \\ Aprsme.PubSub) do
    task =
      Task.Supervisor.async_nolink(@pool_name, fn ->
        # Use Stream for memory efficiency with large topic lists
        topics
        # Process in chunks to balance load
        |> Stream.chunk_every(10)
        |> Enum.each(fn topic_chunk ->
          broadcast_to_topics(topic_chunk, message, pubsub)
        end)
      end)

    {:ok, task}
  end

  @doc """
  Broadcasts a single message to a topic asynchronously.
  """
  def broadcast_one_async(topic, message, pubsub \\ Aprsme.PubSub) do
    Task.Supervisor.start_child(@pool_name, fn ->
      Phoenix.PubSub.broadcast(pubsub, topic, message)
    end)
  end

  @doc """
  Executes a function asynchronously in the broadcast pool.

  This is useful for any broadcast-related async operation.
  """
  def async_execute(fun) when is_function(fun, 0) do
    Task.Supervisor.start_child(@pool_name, fun)
  end

  @doc """
  Gets statistics about the broadcast pool.
  """
  def get_stats do
    children = Task.Supervisor.children(@pool_name)

    %{
      active_tasks: length(children),
      pool_size: :persistent_term.get({__MODULE__, :pool_size}, 0),
      scheduler_usage: scheduler_usage()
    }
  end

  # Calculate approximate scheduler usage
  defp scheduler_usage do
    1
    |> :scheduler.utilization()
    |> Enum.map(fn {_, usage, _} -> usage end)
    |> Enum.sum()
    |> Kernel./(System.schedulers_online())
    |> Float.round(2)
  rescue
    _ -> 0.0
  end

  # Private helper to broadcast to multiple topics
  defp broadcast_to_topics(topics, message, pubsub) do
    Enum.each(topics, fn topic ->
      Phoenix.PubSub.broadcast(pubsub, topic, message)
    end)
  end
end

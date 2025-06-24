defmodule Aprsme.Archiver do
  @moduledoc false
  use GenServer

  alias AprsmeWeb.Endpoint

  require Jason
  require Logger

  # alias Aprsme.{Packet, Repo}
  @topic "call"

  # API
  @spec start_link(any) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(_args \\ []) do
    GenServer.start_link(__MODULE__, [], name: :archiver)
  end

  # Callbacks

  @spec init(any) :: {:ok, any}
  def init(state \\ []) do
    Process.send_after(self(), :connect, 5000)
    Endpoint.subscribe(@topic)
    {:ok, state}
  end

  def handle_info(:connect, state) do
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
end

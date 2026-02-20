defmodule Aprsme.Workers.PacketCleanupWorker do
  @moduledoc """
  Worker for cleaning up old APRS packet data.

  Delegates to `PartitionManager.drop_old_partitions/1` which drops entire
  daily partitions past the retention period — instant, no dead tuples, no VACUUM.

  Also cleans up old `bad_packets` records via standard DELETE.
  """

  import Ecto.Query

  alias Aprsme.BadPacket
  alias Aprsme.PartitionManager
  alias Aprsme.Repo

  require Logger

  @spec perform(map()) :: :ok | {:error, String.t()}
  def perform(%{"cleanup_days" => days}) when is_integer(days) and days > 0 do
    Logger.info("Starting scheduled APRS packet cleanup for packets older than #{days} days")

    {:ok, dropped} = PartitionManager.drop_old_partitions(days)

    Logger.info("APRS packet cleanup complete: dropped #{length(dropped)} partition(s) older than #{days} days")

    :ok
  rescue
    error ->
      Logger.error("Packet cleanup failed: #{inspect(error)}\n#{inspect(__STACKTRACE__)}")
      {:error, "Cleanup failed: #{inspect(error)}"}
  end

  @spec perform(map()) :: :ok | {:error, String.t()}
  def perform(_args) do
    Logger.info("Starting scheduled APRS packet cleanup")

    retention_days = Application.get_env(:aprsme, :packet_retention_days, 7)

    {:ok, dropped} = PartitionManager.drop_old_partitions(retention_days)
    bad_packet_count = cleanup_old_bad_packets()

    Logger.info(
      "APRS packet cleanup complete: dropped #{length(dropped)} partition(s) and removed #{bad_packet_count} bad packets older than #{retention_days} days"
    )

    :ok
  rescue
    error ->
      Logger.error("Packet cleanup failed: #{inspect(error)}\n#{inspect(__STACKTRACE__)}")
      {:error, "Cleanup failed: #{inspect(error)}"}
  end

  defp cleanup_old_bad_packets do
    retention_days = Application.get_env(:aprsme, :packet_retention_days, 7)
    cutoff_time = DateTime.add(DateTime.utc_now(), -retention_days * 86_400, :second)

    {deleted_count, _} =
      Repo.delete_all(from(b in BadPacket, where: b.attempted_at < ^cutoff_time))

    deleted_count
  rescue
    error ->
      Logger.error("Failed to clean up bad packets: #{inspect(error)}")
      0
  end
end

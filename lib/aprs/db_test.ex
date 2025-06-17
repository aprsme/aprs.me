defmodule Aprs.DbTest do
  @moduledoc """
  Helper module for testing database connections and operations from IEx.
  Run with:
    iex> Aprs.DbTest.test_packet_storage()
  """

  alias Aprs.Packet
  alias Aprs.Packets
  alias Aprs.Repo

  require Logger

  @doc """
  Tests packet storage functionality by creating and storing a test packet.
  """
  def test_packet_storage do
    IO.puts("Starting database test...")

    # Test database connection
    case check_db_connection() do
      :ok ->
        # Create a test packet with minimum required fields
        test_packet = %{
          base_callsign: "TEST",
          ssid: "1",
          sender: "TEST-1",
          destination: "APRS",
          data_type: "position",
          path: "TCPIP*",
          information_field: "Test packet",
          received_at: DateTime.utc_now(),
          lat: 33.5,
          lon: -97.5,
          region: "test",
          has_position: true,
          data_extended: %{
            latitude: 33.5,
            longitude: -97.5,
            symbol_table_id: "/",
            symbol_code: ">",
            comment: "Test comment",
            aprs_messaging: false
          }
        }

        IO.puts("Created test packet: #{inspect(test_packet, pretty: true)}")

        # Attempt to store the packet
        case Packets.store_packet(test_packet) do
          {:ok, stored_packet} ->
            {:ok, stored_packet}

          {:error, :storage_exception} ->
            IO.puts("Failed to store packet! (storage exception)")
            {:error, :storage_exception}

          {:error, :validation_error} ->
            IO.puts("Failed to store packet! (validation error)")
            {:error, :validation_error}

          {:error, other_error} ->
            IO.puts("Failed to store packet!")
            IO.puts("Error: #{inspect(other_error)}")
            {:error, other_error}
        end

      error ->
        IO.puts("Database connection test failed: #{inspect(error)}")
        error
    end
  end

  @doc """
  Tests if the database is accessible.
  """
  def check_db_connection do
    IO.puts("Checking database connection...")

    try do
      # Try to get a connection from the pool
      Repo.checkout(fn conn ->
        IO.puts("Got database connection!")
        Postgrex.query!(conn, "SELECT 1", [])
      end)

      # Try a simple query through Ecto
      count = Repo.aggregate(Packet, :count, :id)
      IO.puts("Current packet count in database: #{count}")

      :ok
    rescue
      e ->
        IO.puts("Database connection error: #{inspect(e)}")
        {:error, e}
    end
  end

  @doc """
  Count packets with position data.
  """
  def count_packets_with_position do
    import Ecto.Query

    IO.puts("Counting packets with position data...")

    try do
      count = Repo.aggregate(from(p in Packet, where: p.has_position == true), :count, :id)

      IO.puts("Found #{count} packets with position data")
      {:ok, count}
    rescue
      e ->
        IO.puts("Query error: #{inspect(e)}")
        {:error, e}
    end
  end

  @doc """
  List the most recent packets with position data.
  """
  def list_recent_packets(limit \\ 10) do
    import Ecto.Query

    IO.puts("Fetching #{limit} most recent packets...")

    try do
      packets =
        Repo.all(
          from(p in Packet,
            where: p.has_position == true,
            order_by: [desc: p.received_at],
            limit: ^limit,
            select: %{
              p
              | lat: fragment("ST_Y(?)", p.location),
                lon: fragment("ST_X(?)", p.location)
            }
          )
        )

      IO.puts("Found #{length(packets)} recent packets")

      if length(packets) > 0 do
        Enum.each(packets, fn packet ->
          IO.puts("#{packet.sender} at #{packet.lat}, #{packet.lon} - #{packet.received_at}")
        end)
      end

      {:ok, packets}
    rescue
      e ->
        IO.puts("Query error: #{inspect(e)}")
        {:error, e}
    end
  end
end

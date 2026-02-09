defmodule Aprsme.ErrorHandlerTest do
  use ExUnit.Case, async: true

  alias Aprsme.ErrorHandler

  # ── helpers to build error structs without hitting complex exception/1 ──

  defp cast_error(message \\ "cannot cast value") do
    %Ecto.Query.CastError{type: :string, value: 42, message: message}
  end

  defp constraint_error(constraint \\ "users_email_index") do
    %Ecto.ConstraintError{type: :unique, constraint: constraint, message: "constraint error"}
  end

  defp postgrex_unique_violation do
    %Postgrex.Error{postgres: %{code: :unique_violation}, message: nil, connection_id: nil}
  end

  defp db_connection_error do
    %DBConnection.ConnectionError{message: "connection refused", severity: :error, reason: :error}
  end

  defp query_error(message \\ "invalid query") do
    %Ecto.QueryError{message: message}
  end

  defp transport_error(reason) do
    %Req.TransportError{reason: reason}
  end

  # ── handle_database_error/2 ──

  describe "handle_database_error/2" do
    test "CastError returns {:error, :invalid_data}" do
      assert {:error, :invalid_data} = ErrorHandler.handle_database_error(cast_error())
    end

    test "ConstraintError returns {:error, :constraint_violation}" do
      assert {:error, :constraint_violation} =
               ErrorHandler.handle_database_error(constraint_error())
    end

    test "Postgrex.Error with unique_violation returns {:error, :duplicate_record}" do
      assert {:error, :duplicate_record} =
               ErrorHandler.handle_database_error(postgrex_unique_violation())
    end

    test "DBConnection.ConnectionError returns {:error, :database_unavailable}" do
      assert {:error, :database_unavailable} =
               ErrorHandler.handle_database_error(db_connection_error())
    end

    test "Ecto.QueryError returns {:error, :query_failed}" do
      assert {:error, :query_failed} = ErrorHandler.handle_database_error(query_error())
    end

    test "catch-all returns {:error, :database_error}" do
      assert {:error, :database_error} =
               ErrorHandler.handle_database_error(%RuntimeError{message: "boom"})
    end

    test "passes context through" do
      assert {:error, :invalid_data} =
               ErrorHandler.handle_database_error(cast_error(), %{table: "users"})
    end
  end

  # ── handle_network_error/3 ──

  describe "handle_network_error/3" do
    test "Req.TransportError with :timeout returns {:error, :timeout}" do
      assert {:error, :timeout} =
               ErrorHandler.handle_network_error(transport_error(:timeout), :aprs_is)
    end

    test "Req.TransportError with :econnrefused returns {:error, :service_unavailable}" do
      assert {:error, :service_unavailable} =
               ErrorHandler.handle_network_error(transport_error(:econnrefused), :aprs_is)
    end

    test "Req.TransportError with other reason returns {:error, :network_error}" do
      assert {:error, :network_error} =
               ErrorHandler.handle_network_error(transport_error(:nxdomain), :geocoder)
    end

    test "{:error, :circuit_open} returns {:error, :service_unavailable}" do
      assert {:error, :service_unavailable} =
               ErrorHandler.handle_network_error({:error, :circuit_open}, :weather_api)
    end

    test "catch-all returns {:error, :network_error}" do
      assert {:error, :network_error} =
               ErrorHandler.handle_network_error(:some_unknown_error, :some_service)
    end

    test "passes context through" do
      assert {:error, :timeout} =
               ErrorHandler.handle_network_error(
                 transport_error(:timeout),
                 :aprs_is,
                 %{attempt: 1}
               )
    end
  end

  # ── handle_packet_error/3 ──

  describe "handle_packet_error/3" do
    test "Jason.DecodeError returns {:error, :invalid_json}" do
      error = %Jason.DecodeError{position: 0, token: nil, data: "bad"}

      assert {:error, :invalid_json} =
               ErrorHandler.handle_packet_error(error, %{sender: "N0CALL"})
    end

    test "ArgumentError with 'invalid' in message returns {:error, :invalid_format}" do
      error = %ArgumentError{message: "invalid byte sequence"}

      assert {:error, :invalid_format} =
               ErrorHandler.handle_packet_error(error, %{sender: "N0CALL"})
    end

    test "ArgumentError without 'invalid' in message returns {:error, :processing_failed}" do
      error = %ArgumentError{message: "bad argument"}

      assert {:error, :processing_failed} =
               ErrorHandler.handle_packet_error(error, %{sender: "N0CALL"})
    end

    test "catch-all returns {:error, :processing_failed}" do
      error = %RuntimeError{message: "something went wrong"}

      assert {:error, :processing_failed} =
               ErrorHandler.handle_packet_error(error, %{sender: "N0CALL"})
    end

    test "uses 'unknown' when packet_data has no :sender key" do
      error = %Jason.DecodeError{position: 0, token: nil, data: "bad"}

      assert {:error, :invalid_json} = ErrorHandler.handle_packet_error(error, %{})
    end

    test "passes context through" do
      error = %Jason.DecodeError{position: 0, token: nil, data: "bad"}

      assert {:error, :invalid_json} =
               ErrorHandler.handle_packet_error(error, %{sender: "N0CALL"}, %{source: "is"})
    end
  end

  # ── handle_process_error/3 ──

  describe "handle_process_error/3" do
    test ":timeout returns {:error, :timeout}" do
      assert {:error, :timeout} = ErrorHandler.handle_process_error(:timeout, :my_genserver)
    end

    test "{:noproc, _} returns {:error, :process_unavailable}" do
      assert {:error, :process_unavailable} =
               ErrorHandler.handle_process_error({:noproc, {GenServer, :call, [:pid]}}, :worker)
    end

    test "{:badrpc, reason} returns {:error, :rpc_failed}" do
      assert {:error, :rpc_failed} =
               ErrorHandler.handle_process_error({:badrpc, :nodedown}, :remote_node)
    end

    test "catch-all returns {:error, :process_error}" do
      assert {:error, :process_error} =
               ErrorHandler.handle_process_error(:something_else, :my_process)
    end

    test "passes context through" do
      assert {:error, :timeout} =
               ErrorHandler.handle_process_error(:timeout, :my_genserver, %{call: :get_state})
    end
  end

  # ── with_error_handling/2 ──

  describe "with_error_handling/2" do
    test "success path returns {:ok, value}" do
      assert {:ok, 42} = ErrorHandler.with_error_handling(fn -> 42 end)
    end

    test "success path returns {:ok, value} for complex results" do
      assert {:ok, %{key: "value"}} = ErrorHandler.with_error_handling(fn -> %{key: "value"} end)
    end

    test "raise with no retries goes to categorize_and_handle_error" do
      result =
        ErrorHandler.with_error_handling(fn ->
          raise RuntimeError, "boom"
        end)

      assert {:error, :unknown_error} = result
    end

    test "raise with retries retries before failing" do
      {:ok, counter} = Agent.start_link(fn -> 0 end)

      result =
        ErrorHandler.with_error_handling(
          fn ->
            count = Agent.get_and_update(counter, fn n -> {n, n + 1} end)

            if count < 2 do
              raise RuntimeError, "boom"
            end

            :success
          end,
          max_retries: 2,
          retry_delay: 1
        )

      assert {:ok, :success} = result

      final_count = Agent.get(counter, & &1)
      assert final_count == 3

      Agent.stop(counter)
    end

    test "raise with retries exhausted calls categorize_and_handle_error" do
      {:ok, counter} = Agent.start_link(fn -> 0 end)

      result =
        ErrorHandler.with_error_handling(
          fn ->
            Agent.update(counter, &(&1 + 1))
            raise RuntimeError, "always fails"
          end,
          max_retries: 2,
          retry_delay: 1
        )

      assert {:error, :unknown_error} = result

      # Initial attempt + 2 retries = 3 total calls
      final_count = Agent.get(counter, & &1)
      assert final_count == 3

      Agent.stop(counter)
    end

    test "throw path returns {:error, :unexpected_error}" do
      result =
        ErrorHandler.with_error_handling(fn ->
          throw(:something_bad)
        end)

      assert {:error, :unexpected_error} = result
    end

    test "database error routes through handle_database_error" do
      result =
        ErrorHandler.with_error_handling(fn ->
          raise DBConnection.ConnectionError, "connection refused"
        end)

      assert {:error, :database_unavailable} = result
    end

    test "network error routes through handle_network_error" do
      result =
        ErrorHandler.with_error_handling(fn ->
          raise Req.TransportError, reason: :timeout
        end)

      assert {:error, :timeout} = result
    end

    test "uncategorized error returns {:error, :unknown_error}" do
      result =
        ErrorHandler.with_error_handling(fn ->
          raise ArgumentError, "nope"
        end)

      assert {:error, :unknown_error} = result
    end

    test "accepts context option" do
      assert {:ok, :fine} =
               ErrorHandler.with_error_handling(fn -> :fine end, context: %{source: "test"})
    end
  end
end

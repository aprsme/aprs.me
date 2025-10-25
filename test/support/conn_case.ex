defmodule AprsmeWeb.ConnCase do
  @moduledoc """
  This module defines the test case to be used by
  tests that require setting up a connection.

  Such tests rely on `Phoenix.ConnTest` and also
  import other functionality to make it easier
  to build common data structures and query the data layer.

  Finally, if the test case interacts with the database,
  we enable the SQL sandbox, so changes done to the database
  are reverted at the end of every test. If you use PostgreSQL, you
  can even run database tests asynchronously by setting
  `use AprsmeWeb.ConnCase, async: true`, although
  this option is not recommended for other databases.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      # The default import for connections
      use AprsmeWeb, :verified_routes

      import Aprsme.MockHelpers
      import AprsmeWeb.ConnCase
      import Phoenix.ConnTest
      import Phoenix.LiveViewTest
      import Plug.Conn

      alias Aprsme.Repo

      # The default endpoint for testing
      @endpoint AprsmeWeb.Endpoint

      # Import conveniences for testing with connections

      # The default import for Repo

      # Helper for LiveView tests that may have duplicate IDs
      def live_with_warn(conn, path) do
        live(conn, path, on_error: :warn)
      end
    end
  end

  setup tags do
    Aprsme.DataCase.setup_sandbox(tags)
    {:ok, conn: Phoenix.ConnTest.build_conn()}
  end

  @doc """
  A helper that sets up the test case.

      use AprsmeWeb.ConnCase, async: true

  """
  def setup_sandbox(_tags) do
    Aprsme.MockHelpers.stub_packets_mock()
    :ok
  end

  @doc """
  A helper that logs in a user.

      setup %{conn: conn} do
        conn = log_in_user(conn, user)
        {:ok, conn: conn}
      end

  """
  def log_in_user(conn, user) do
    token = Aprsme.Accounts.generate_user_session_token(user)

    conn
    |> Phoenix.ConnTest.init_test_session(%{})
    |> Plug.Conn.put_session(:user_token, token)
    |> Plug.Conn.put_session(:live_socket_id, "users_sessions:#{Base.url_encode64(token)}")
  end
end

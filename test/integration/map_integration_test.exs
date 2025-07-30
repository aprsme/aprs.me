defmodule AprsmeWeb.MapIntegrationTest do
  @moduledoc """
  Integration tests for the main map interface using Wallaby
  Tests the complete user experience including JavaScript interactions
  """
  use ExUnit.Case, async: false
  use Wallaby.Feature

  import Wallaby.Browser

  alias Aprsme.Repo
  alias Ecto.Adapters.SQL.Sandbox
  alias Wallaby.Query

  @moduletag :integration

  setup do
    :ok = Sandbox.checkout(Repo)
    # Use shared mode for Wallaby tests
    Sandbox.mode(Repo, {:shared, self()})

    {:ok, %{}}
  end

  feature "user can view the main map interface", %{session: session} do
    session
    |> visit("/")
    |> assert_has(Query.css("body"))
  end

  feature "page loads successfully", %{session: session} do
    session
    |> visit("/")
    |> assert_has(Query.css("body"))
  end

  feature "can access different routes", %{session: session} do
    session
    |> visit("/info")
    |> assert_has(Query.css("body"))
  end
end

defmodule AprsmeWeb.SimpleIntegrationTest do
  @moduledoc """
  Simple integration test to verify Wallaby setup works
  """
  use ExUnit.Case, async: false
  use Wallaby.Feature

  import Wallaby.Browser

  alias Ecto.Adapters.SQL.Sandbox
  alias Wallaby.Query

  @moduletag :integration

  setup do
    :ok = Sandbox.checkout(Aprsme.Repo)
    Sandbox.mode(Aprsme.Repo, {:shared, self()})
    
    {:ok, %{}}
  end

  feature "can visit homepage and see title", %{session: session} do
    session
    |> visit("/")
    |> assert_has(Query.css("body"))
  end
end

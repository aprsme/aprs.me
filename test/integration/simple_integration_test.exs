defmodule AprsmeWeb.SimpleIntegrationTest do
  @moduledoc """
  Simple integration test to verify Wallaby setup works
  """
  use ExUnit.Case, async: false
  use Wallaby.Feature

  import Wallaby.Browser
  
  alias Wallaby.Query

  @moduletag :integration

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Aprsme.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Aprsme.Repo, {:shared, self()})
    {:ok, %{}}
  end

  feature "can visit homepage and see title", %{session: session} do
    session
    |> visit("/")
    |> assert_has(Query.text("APRS"))
  end
end
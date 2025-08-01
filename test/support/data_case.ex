defmodule Aprsme.DataCase do
  @moduledoc """
  This module defines the test case to be used by
  data tests.

  You may define functions here to be used as helpers in
  your data tests. See `errors_on/2`'s definition as an example.

  Finally, if the test case interacts with the database,
  we enable the SQL sandbox, so changes done to the database
  are reverted at the end of every test. If you use PostgreSQL, you
  can even run database tests asynchronously by setting
  `use Aprsme.DataCase, async: true`, although
  this option is not recommended for other databases.
  """

  use ExUnit.CaseTemplate

  alias Ecto.Adapters.SQL.Sandbox

  using do
    quote do
      import Aprsme.DataCase
      # Import conveniences for testing with connections
      import Ecto
      import Ecto.Changeset
      import Ecto.Query

      # and other functionality to make calls such as:
      #     import Aprsme.DataCase
      #     Aprsme.DataCase.errors_on(MySchema.changeset(%MySchema{}, %{}))

      # The default import for Repo
      alias Aprsme.Repo
    end
  end

  setup tags do
    Aprsme.DataCase.setup_sandbox(tags)
    # Only seed devices for tests that need them
    if tags[:needs_devices] do
      Aprsme.DevicesSeeder.seed_from_json()
    end
    :ok
  end

  @doc """
  A helper that transforms changeset errors into a map of messages.

      iex> errors_on(MySchema.changeset(%MySchema{}, %{field: bad_value}))
      %{field: ["has invalid value"]}

  """
  def errors_on(changeset) do
    Ecto.Changeset.traverse_errors(changeset, &translate_error/1)
  end

  @doc """
  Sets up the sandbox and allows the test case
  to be run asynchronously.
  """
  def setup_sandbox(tags) do
    pid = Sandbox.start_owner!(Aprsme.Repo, shared: not tags[:async])
    on_exit(fn -> Sandbox.stop_owner(pid) end)
  end

  defp translate_error({msg, opts}) do
    # You can make use of gettext to translate error messages by
    # uncommenting and adjusting the following code:

    # if count = opts[:count] do
    #   Gettext.dngettext(AprsmeWeb.Gettext, "errors", msg, msg, count, opts)
    # else
    #   Gettext.dgettext(AprsmeWeb.Gettext, "errors", msg, opts)
    # end

    Enum.reduce(opts, msg, fn {key, value}, acc ->
      String.replace(acc, "%{#{key}}", fn _ -> to_string(value) end)
    end)
  end
end

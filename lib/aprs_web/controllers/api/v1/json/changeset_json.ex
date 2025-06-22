defmodule AprsWeb.Api.V1.ChangesetJSON do
  @moduledoc """
  Renders changeset errors for API v1.
  """

  @doc """
  Renders changeset errors.
  """
  def render("error.json", %{changeset: changeset}) do
    %{
      error: %{
        message: "Validation failed",
        code: "validation_error",
        details: translate_errors(changeset)
      }
    }
  end

  defp translate_errors(changeset) do
    Ecto.Changeset.traverse_errors(changeset, &translate_error/1)
  end

  defp translate_error({msg, opts}) do
    # Because the error messages we show in our forms and APIs
    # are defined inside Ecto, we need to translate them dynamically.
    Enum.reduce(opts, msg, fn {key, value}, acc ->
      String.replace(acc, "%{#{key}}", to_string(value))
    end)
  end
end

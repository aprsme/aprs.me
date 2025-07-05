defmodule AprsmeWeb.Api.V1.FallbackController do
  @moduledoc """
  Translates controller action results into valid `Plug.Conn` responses.

  See `Phoenix.Controller.action_fallback/1` for more details.
  """
  use AprsmeWeb, :controller
  use Gettext, backend: AprsmeWeb.Gettext

  alias AprsmeWeb.Api.V1.ErrorJSON

  # This clause handles errors returned by Ecto's insert/update/delete.
  def call(conn, {:error, %Ecto.Changeset{} = changeset}) do
    conn
    |> put_status(:unprocessable_entity)
    |> put_view(json: AprsmeWeb.Api.V1.ChangesetJSON)
    |> render(:error, changeset: changeset)
  end

  # This clause is an example of how to handle resources that cannot be found.
  def call(conn, {:error, :not_found}) do
    conn
    |> put_status(:not_found)
    |> put_view(json: ErrorJSON)
    |> render(:"404")
  end

  # Handle generic errors
  def call(conn, {:error, reason}) when is_atom(reason) do
    status =
      case reason do
        :unauthorized -> :unauthorized
        :forbidden -> :forbidden
        :bad_request -> :bad_request
        _ -> :internal_server_error
      end

    conn
    |> put_status(status)
    |> put_view(json: ErrorJSON)
    |> render(:error, message: format_error_message(reason))
  end

  # Handle string error messages
  def call(conn, {:error, message}) when is_binary(message) do
    conn
    |> put_status(:bad_request)
    |> put_view(json: ErrorJSON)
    |> render(:error, message: message)
  end

  # Handle timeout errors
  def call(conn, :timeout) do
    conn
    |> put_status(:request_timeout)
    |> put_view(json: ErrorJSON)
    |> render(:error, message: gettext("Request timeout"))
  end

  # Default fallback for unexpected errors
  def call(conn, _error) do
    conn
    |> put_status(:internal_server_error)
    |> put_view(json: ErrorJSON)
    |> render(:"500")
  end

  defp format_error_message(:not_found), do: gettext("Resource not found")
  defp format_error_message(:unauthorized), do: gettext("Unauthorized access")
  defp format_error_message(:forbidden), do: gettext("Access forbidden")
  defp format_error_message(:bad_request), do: gettext("Bad request")
  defp format_error_message(reason), do: gettext("An error occurred: %{reason}", reason: reason)
end

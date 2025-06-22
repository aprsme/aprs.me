defmodule AprsWeb.Api.V1.FallbackController do
  @moduledoc """
  Translates controller action results into valid `Plug.Conn` responses.

  See `Phoenix.Controller.action_fallback/1` for more details.
  """
  use AprsWeb, :controller

  alias AprsWeb.Api.V1.ErrorJSON

  # This clause handles errors returned by Ecto's insert/update/delete.
  def call(conn, {:error, %Ecto.Changeset{} = changeset}) do
    conn
    |> put_status(:unprocessable_entity)
    |> put_view(json: AprsWeb.Api.V1.ChangesetJSON)
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
    |> render(:error, message: "Request timeout")
  end

  # Default fallback for unexpected errors
  def call(conn, _error) do
    conn
    |> put_status(:internal_server_error)
    |> put_view(json: ErrorJSON)
    |> render(:"500")
  end

  defp format_error_message(:not_found), do: "Resource not found"
  defp format_error_message(:unauthorized), do: "Unauthorized access"
  defp format_error_message(:forbidden), do: "Access forbidden"
  defp format_error_message(:bad_request), do: "Bad request"
  defp format_error_message(reason), do: "An error occurred: #{reason}"
end

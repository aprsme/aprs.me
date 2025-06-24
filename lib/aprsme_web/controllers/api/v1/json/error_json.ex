defmodule AprsmeWeb.Api.V1.ErrorJSON do
  @moduledoc """
  Renders error responses for API v1.
  """

  def render("error.json", %{message: message}) do
    %{
      error: %{
        message: message,
        code: "generic_error"
      }
    }
  end

  def render("404.json", _assigns) do
    %{
      error: %{
        message: "Resource not found",
        code: "not_found"
      }
    }
  end

  def render("500.json", _assigns) do
    %{
      error: %{
        message: "Internal server error",
        code: "internal_server_error"
      }
    }
  end

  def render("422.json", _assigns) do
    %{
      error: %{
        message: "Unprocessable entity",
        code: "unprocessable_entity"
      }
    }
  end

  def render("400.json", _assigns) do
    %{
      error: %{
        message: "Bad request",
        code: "bad_request"
      }
    }
  end

  def render("401.json", _assigns) do
    %{
      error: %{
        message: "Unauthorized",
        code: "unauthorized"
      }
    }
  end

  def render("403.json", _assigns) do
    %{
      error: %{
        message: "Forbidden",
        code: "forbidden"
      }
    }
  end

  def render("408.json", _assigns) do
    %{
      error: %{
        message: "Request timeout",
        code: "request_timeout"
      }
    }
  end

  def render(template, _assigns) do
    %{
      error: %{
        message: Phoenix.Controller.status_message_from_template(template),
        code: "unknown_error"
      }
    }
  end
end

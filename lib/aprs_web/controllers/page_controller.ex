defmodule AprsWeb.PageController do
  use AprsWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end

  def map(conn, _params) do
    render(conn, :map)
  end

  def packets(conn, _params) do
    render(conn, :packets)
  end

  def health(conn, _params) do
    json(conn, %{status: "ok", version: Application.spec(:aprs, :vsn)})
  end
end

defmodule AprsWeb.PageController do
  use AprsWeb, :controller

  def home(conn, _params) do
    # The home page is often custom made,
    # so skip the default app layout.
    render(conn, :home, layout: false)
  end

  def map(conn, _params) do
    render(conn, :map, layout: false)
  end
end

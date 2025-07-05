defmodule AprsmeWeb.AboutLive do
  @moduledoc false
  use AprsmeWeb, :live_view
  use Gettext, backend: AprsmeWeb.Gettext

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, page_title: "About aprs.me")}
  end
end

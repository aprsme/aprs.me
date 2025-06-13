defmodule AprsWeb.Layouts do
  @moduledoc false
  use AprsWeb, :html

  embed_templates "layouts/*"

  def body_class(assigns) do
    classes = ["bg-white antialiased"]

    # Check if this is the home page (map)
    is_home =
      cond do
        # For regular controller requests
        assigns[:conn] && assigns.conn.request_path == "/" -> true
        # For LiveView requests
        assigns[:socket] && assigns.socket.view == AprsWeb.MapLive.Index -> true
        true -> false
      end

    if is_home, do: ["home-page" | classes], else: classes
  end
end

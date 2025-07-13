defmodule AprsmeWeb.RedirectController do
  use AprsmeWeb, :controller

  def callsign_to_map(conn, %{"callsign" => callsign}) do
    redirect(conn, to: "/?call=#{callsign}")
  end
end

defmodule AprsmeWeb.LocaleHook do
  @moduledoc """
  LiveView hook for setting locale based on Accept-Language header.
  This ensures that LiveView updates maintain the correct locale.
  """
  import Phoenix.Component

  # Cache supported locales at compile time
  @supported_locales ~w(en es de fr)

  def on_mount(:set_locale, params, session, socket) do
    # Only set locale in production to allow seeing "xx" placeholders in dev
    locale =
      if Application.get_env(:aprsme, :env) == :prod do
        get_locale_from_session(session) || "en"
      end

    if locale do
      Gettext.put_locale(AprsmeWeb.Gettext, locale)
    end

    # Set map_page assign based on the view module
    map_page = is_map_page?(socket, params)

    {:cont, assign(socket, locale: locale, map_page: map_page)}
  end

  defp get_locale_from_session(session) do
    case session do
      %{"locale" => locale} when locale in @supported_locales ->
        locale

      _ ->
        nil
    end
  end

  defp is_map_page?(socket, _params) do
    # Only check the view module from socket private data
    case socket.private[:phoenix_live_view] do
      %{view: AprsmeWeb.MapLive.Index} -> true
      %{view: AprsmeWeb.MapLive.CallsignView} -> true
      _ -> false
    end
  end
end

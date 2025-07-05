defmodule AprsmeWeb.LocaleHook do
  @moduledoc """
  LiveView hook for setting locale based on Accept-Language header.
  This ensures that LiveView updates maintain the correct locale.
  """
  import Phoenix.Component

  @supported_locales ["en", "es"]

  def on_mount(:set_locale, _params, session, socket) do
    locale = get_locale_from_session(session) || "en"
    Gettext.put_locale(AprsmeWeb.Gettext, locale)
    {:cont, assign(socket, locale: locale)}
  end

  defp get_locale_from_session(session) do
    case session do
      %{"locale" => locale} when locale in @supported_locales ->
        locale

      _ ->
        nil
    end
  end
end

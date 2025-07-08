defmodule AprsmeWeb.LocaleHook do
  @moduledoc """
  LiveView hook for setting locale based on Accept-Language header.
  This ensures that LiveView updates maintain the correct locale.
  """
  import Phoenix.Component

  # Cache supported locales at compile time
  @supported_locales ~w(en es de fr)

  def on_mount(:set_locale, params, session, socket) do
    locale = get_locale_from_session(session) || "en"
    Gettext.put_locale(AprsmeWeb.Gettext, locale)

    # Set map_page assign based on route
    map_page = is_map_page?(params)

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

  defp is_map_page?(%{"callsign" => callsign}) when map_size(%{"callsign" => callsign}) == 1 do
    # Root callsign route (e.g., /:callsign) is a map page
    true
  end

  defp is_map_page?(%{}) do
    # Root route (/) is a map page
    true
  end

  defp is_map_page?(_) do
    false
  end
end

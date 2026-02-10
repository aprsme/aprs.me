defmodule AprsmeWeb.Plugs.SetLocale do
  @moduledoc """
  A plug that sets the locale based on the Accept-Language header.
  Falls back to English if the requested locale is not available.
  """
  import Plug.Conn

  def init(opts), do: opts

  def call(conn, _opts) do
    locale = get_locale_from_header(conn) || "en"
    # Set the backend's locale for the current process
    Gettext.put_locale(AprsmeWeb.Gettext, locale)
    # Store locale in session for LiveView to access
    conn = put_session(conn, :locale, locale)
    conn
  end

  defp get_locale_from_header(conn) do
    conn
    |> get_req_header("accept-language")
    |> extract_locale()
  end

  defp extract_locale([accept_language | _]), do: parse_accept_language(accept_language)
  defp extract_locale(_), do: nil

  defp parse_accept_language(accept_language) do
    accept_language
    |> String.split(",")
    |> Enum.map(&parse_language_tag/1)
    |> Enum.find(&supported_locale?/1)
  end

  defp parse_language_tag(tag) do
    tag
    |> String.trim()
    |> String.split(";")
    |> List.first()
    |> String.split("-")
    |> List.first()
    |> String.downcase()
  end

  defp supported_locale?(locale) do
    locale in ~w(en es fr de)
  end
end

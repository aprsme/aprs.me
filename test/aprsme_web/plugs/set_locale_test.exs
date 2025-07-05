defmodule AprsmeWeb.Plugs.SetLocaleTest do
  use AprsmeWeb.ConnCase

  alias AprsmeWeb.Plugs.SetLocale

  test "sets Spanish locale when Accept-Language header contains es", %{conn: conn} do
    conn
    |> Plug.Session.call(Plug.Session.init(store: :cookie, key: "_aprs_key", signing_salt: "test"))
    |> fetch_session()
    |> put_req_header("accept-language", "es-ES,es;q=0.9,en;q=0.8")
    |> SetLocale.call([])

    assert Gettext.get_locale(AprsmeWeb.Gettext) == "es"
  end

  test "sets English locale when Accept-Language header contains en", %{conn: conn} do
    conn
    |> Plug.Session.call(Plug.Session.init(store: :cookie, key: "_aprs_key", signing_salt: "test"))
    |> fetch_session()
    |> put_req_header("accept-language", "en-US,en;q=0.9,es;q=0.8")
    |> SetLocale.call([])

    assert Gettext.get_locale(AprsmeWeb.Gettext) == "en"
  end

  test "falls back to English when Accept-Language header contains unsupported locale", %{conn: conn} do
    conn
    |> Plug.Session.call(Plug.Session.init(store: :cookie, key: "_aprs_key", signing_salt: "test"))
    |> fetch_session()
    |> put_req_header("accept-language", "fr-FR,fr;q=0.9,en;q=0.8")
    |> SetLocale.call([])

    assert Gettext.get_locale(AprsmeWeb.Gettext) == "en"
  end

  test "falls back to English when no Accept-Language header is present", %{conn: conn} do
    conn
    |> Plug.Session.call(Plug.Session.init(store: :cookie, key: "_aprs_key", signing_salt: "test"))
    |> fetch_session()
    |> SetLocale.call([])

    assert Gettext.get_locale(AprsmeWeb.Gettext) == "en"
  end

  test "prefers first supported locale in Accept-Language header", %{conn: conn} do
    conn
    |> Plug.Session.call(Plug.Session.init(store: :cookie, key: "_aprs_key", signing_salt: "test"))
    |> fetch_session()
    |> put_req_header("accept-language", "fr-FR,es-ES,en-US;q=0.9")
    |> SetLocale.call([])

    assert Gettext.get_locale(AprsmeWeb.Gettext) == "es"
  end
end

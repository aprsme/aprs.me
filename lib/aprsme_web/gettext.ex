defmodule AprsmeWeb.Gettext do
  @moduledoc """
  A module providing Internationalization with a gettext-based API.

  By using [Gettext](https://hexdocs.pm/gettext),
  your module gains a set of macros for translations, for example:

      import AprsmeWeb.Gettext

      # Simple translation
      gettext("Here is the string to translate")

      # Plural translation
      ngettext("Here is the string to translate",
               "Here are the strings to translate",
               3)

      # Domain-based translation
      dgettext("errors", "Here is the error message to translate")

  See the [Gettext Docs](https://hexdocs.pm/gettext) for detailed usage.
  """
  use Gettext.Backend, otp_app: :aprsme

  @supported_locales ~w(en es de fr)

  @doc """
  Sets the Gettext locale for the current process.
  """
  def put_locale(locale) when locale in @supported_locales do
    Gettext.put_locale(__MODULE__, locale)
  end

  @doc """
  Returns the list of supported locales.
  """
  def supported_locales, do: @supported_locales
end

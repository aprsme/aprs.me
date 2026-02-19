defmodule AprsmeWeb.Layouts do
  @moduledoc false
  use AprsmeWeb, :html

  embed_templates "layouts/*"

  def body_class(_assigns) do
    ["bg-white dark:bg-gray-900 text-gray-900 dark:text-white antialiased"]
  end
end

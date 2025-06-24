defmodule AprsmeWeb.Layouts do
  @moduledoc false
  use AprsmeWeb, :html

  embed_templates "layouts/*"

  def body_class(_assigns) do
    ["bg-white antialiased"]
  end
end

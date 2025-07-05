defmodule AprsmeWeb.Layouts do
  @moduledoc false
  use AprsmeWeb, :html

  embed_templates "layouts/*"

  def body_class(_assigns) do
    ["bg-base-100 antialiased"]
  end
end

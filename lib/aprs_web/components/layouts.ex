defmodule AprsWeb.Layouts do
  @moduledoc false
  use AprsWeb, :html

  embed_templates "layouts/*"

  def body_class(_assigns) do
    ["bg-white antialiased"]
  end
end

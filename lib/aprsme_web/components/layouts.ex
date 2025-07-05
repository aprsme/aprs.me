defmodule AprsmeWeb.Layouts do
  @moduledoc false
  use AprsmeWeb, :html

  embed_templates "layouts/*"

  def body_class(assigns) do
    base_classes = ["bg-base-100 antialiased"]

    if Map.get(assigns, :map_page, false) do
      base_classes ++ ["map-page"]
    else
      base_classes
    end
  end
end

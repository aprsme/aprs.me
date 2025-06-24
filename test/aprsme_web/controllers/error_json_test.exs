defmodule AprsmeWeb.ErrorJSONTest do
  use AprsmeWeb.ConnCase

  test "renders 404" do
    assert AprsmeWeb.ErrorJSON.render("404.json", %{}) == %{errors: %{detail: "Not Found"}}
  end

  test "renders 500" do
    assert AprsmeWeb.ErrorJSON.render("500.json", %{}) ==
             %{errors: %{detail: "Internal Server Error"}}
  end
end

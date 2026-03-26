defmodule AprsmeWeb.Plugs.ApiCSRFTest do
  use AprsmeWeb.ConnCase, async: true

  alias AprsmeWeb.Plugs.ApiCSRF

  test "allows JSON requests with XMLHttpRequest header", %{conn: conn} do
    conn =
      conn
      |> init_test_session(%{})
      |> put_req_header("content-type", "application/json")
      |> put_req_header("x-requested-with", "XMLHttpRequest")
      |> ApiCSRF.call([])

    refute conn.halted
  end

  test "rejects JSON requests with arbitrary bearer token", %{conn: conn} do
    conn =
      conn
      |> init_test_session(%{})
      |> put_req_header("content-type", "application/json")
      |> put_req_header("authorization", "Bearer definitely-not-valid")
      |> ApiCSRF.call([])

    assert conn.halted
    assert conn.status == 403
    assert Jason.decode!(conn.resp_body)["error"] == "CSRF protection failed"
  end

  test "allows non-JSON requests without CSRF headers", %{conn: conn} do
    conn =
      conn
      |> init_test_session(%{})
      |> put_req_header("content-type", "text/plain")
      |> ApiCSRF.call([])

    refute conn.halted
  end
end

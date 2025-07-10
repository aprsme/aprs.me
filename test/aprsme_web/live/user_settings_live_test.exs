defmodule AprsmeWeb.UserSettingsLiveTest do
  use AprsmeWeb.ConnCase

  import Aprsme.AccountsFixtures
  import Phoenix.LiveViewTest

  alias Aprsme.Accounts

  describe "Settings page" do
    setup %{conn: conn} do
      user = user_fixture()
      conn = log_in_user(conn, user)
      {:ok, conn: conn, user: user}
    end

    test "renders settings page", %{conn: conn} do
      {:ok, _lv, html} = live(conn, ~p"/users/settings")

      assert html =~ "Change Email"
      assert html =~ "Change Callsign"
      assert html =~ "Change Password"
    end

    test "redirects if user is not logged in" do
      conn = build_conn()

      {:error, redirect} = live(conn, ~p"/users/settings")

      assert {:redirect, %{to: path, flash: flash}} = redirect
      assert path == ~p"/users/log_in"
      assert %{"error" => "You must log in to access this page."} = flash
    end
  end

  describe "update callsign form" do
    setup %{conn: conn} do
      user = user_fixture()
      conn = log_in_user(conn, user)
      {:ok, conn: conn, user: user}
    end

    test "updates the user callsign", %{conn: conn, user: user} do
      {:ok, lv, _html} = live(conn, ~p"/users/settings")

      lv
      |> form("#callsign_form", %{
        "current_password" => valid_user_password(),
        "user" => %{"callsign" => "W9NEW"}
      })
      |> render_submit()

      # The form should redirect after success
      flash = assert_redirect(lv, ~p"/users/settings")
      assert flash["info"] == "Callsign updated successfully."

      # Verify the callsign was updated
      updated_user = Accounts.get_user!(user.id)
      assert updated_user.callsign == "W9NEW"
    end

    test "renders errors with invalid callsign", %{conn: conn} do
      {:ok, lv, _html} = live(conn, ~p"/users/settings")

      result =
        lv
        |> form("#callsign_form", %{
          "current_password" => valid_user_password(),
          "user" => %{"callsign" => "INVALID"}
        })
        |> render_submit()

      assert result =~ "must be a valid amateur radio callsign"
    end

    test "renders errors with invalid password", %{conn: conn, user: user} do
      {:ok, lv, _html} = live(conn, ~p"/users/settings")

      result =
        lv
        |> form("#callsign_form", %{
          "current_password" => "invalid",
          "user" => %{"callsign" => "W9NEW"}
        })
        |> render_submit()

      assert result =~ "is not valid"

      # Verify the callsign was not updated
      updated_user = Accounts.get_user!(user.id)
      assert updated_user.callsign == user.callsign
    end
  end

  describe "update email form" do
    setup %{conn: conn} do
      user = user_fixture()
      conn = log_in_user(conn, user)
      {:ok, conn: conn, user: user}
    end

    test "updates the user email", %{conn: conn, user: user} do
      {:ok, lv, _html} = live(conn, ~p"/users/settings")

      result =
        lv
        |> form("#email_form", %{
          "current_password" => valid_user_password(),
          "user" => %{"email" => unique_user_email()}
        })
        |> render_submit()

      assert result =~ "A link to confirm your email"
      assert Accounts.get_user_by_email(user.email)
    end

    test "renders errors with invalid data (phx-change)", %{conn: conn} do
      {:ok, lv, _html} = live(conn, ~p"/users/settings")

      result =
        lv
        |> element("#email_form")
        |> render_change(%{
          "current_password" => valid_user_password(),
          "user" => %{"email" => "with spaces"}
        })

      assert result =~ "must have the @ sign and no spaces"
    end
  end

  describe "update password form" do
    setup %{conn: conn} do
      user = user_fixture()
      conn = log_in_user(conn, user)
      {:ok, conn: conn, user: user}
    end

    test "updates the user password", %{conn: conn, user: user} do
      {:ok, lv, _html} = live(conn, ~p"/users/settings")

      form =
        form(lv, "#password_form", %{
          "current_password" => valid_user_password(),
          "user" => %{
            "email" => user.email,
            "password" => "new valid password",
            "password_confirmation" => "new valid password"
          }
        })

      render_submit(form)

      new_password_conn = follow_trigger_action(form, conn)

      assert redirected_to(new_password_conn) == ~p"/users/settings"

      assert get_session(new_password_conn, :user_token) != get_session(conn, :user_token)

      assert Phoenix.Flash.get(new_password_conn.assigns.flash, :info) =~
               "Password updated successfully"

      assert Accounts.get_user_by_email_and_password(user.email, "new valid password")
    end

    test "renders errors with invalid data (phx-change)", %{conn: conn} do
      {:ok, lv, _html} = live(conn, ~p"/users/settings")

      result =
        lv
        |> element("#password_form")
        |> render_change(%{
          "current_password" => valid_user_password(),
          "user" => %{
            "password" => "short",
            "password_confirmation" => "does not match"
          }
        })

      assert result =~ "should be at least 12 character"
      assert result =~ "does not match password"
    end
  end
end

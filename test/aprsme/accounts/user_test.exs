defmodule Aprsme.Accounts.UserTest do
  use Aprsme.DataCase, async: true

  alias Aprsme.Accounts.User

  describe "registration_changeset/2" do
    test "requires callsign" do
      changeset = User.registration_changeset(%User{}, %{})
      errors = errors_on(changeset)
      assert Map.has_key?(errors, :callsign)
      assert "can't be blank" in errors.callsign
    end

    test "validates callsign format" do
      invalid_callsigns = ["123", "TOOLONGCALLSIGN", "NO SPACES", "nodigit"]

      for callsign <- invalid_callsigns do
        changeset =
          User.registration_changeset(%User{}, %{
            email: "test@example.com",
            password: "valid_password123",
            callsign: callsign
          })

        errors = errors_on(changeset)
        assert Map.has_key?(errors, :callsign), "Expected callsign error for: #{callsign}"
        assert "must be a valid amateur radio callsign" in errors.callsign
      end
    end

    test "validates empty callsign" do
      changeset =
        User.registration_changeset(%User{}, %{
          email: "test@example.com",
          password: "valid_password123",
          callsign: ""
        })

      errors = errors_on(changeset)
      assert Map.has_key?(errors, :callsign)
      assert "can't be blank" in errors.callsign
    end

    test "accepts valid callsigns" do
      valid_callsigns = ["K1ABC", "W2XYZ", "AA3BB", "KG4AAA", "N7PQR", "VE3ABC"]

      for callsign <- valid_callsigns do
        changeset =
          User.registration_changeset(%User{}, %{
            email: "test@example.com",
            password: "valid_password123",
            callsign: callsign
          })

        assert changeset.valid?
      end
    end

    test "converts callsign to uppercase" do
      changeset =
        User.registration_changeset(%User{}, %{
          email: "test@example.com",
          password: "valid_password123",
          callsign: "k1abc"
        })

      assert get_change(changeset, :callsign) == "K1ABC"
    end
  end

  describe "callsign_changeset/2" do
    test "requires callsign to change" do
      user = %User{callsign: "K1ABC"}
      changeset = User.callsign_changeset(user, %{callsign: "K1ABC"})
      errors = errors_on(changeset)
      assert Map.has_key?(errors, :callsign)
      assert "did not change" in errors.callsign
    end

    test "validates new callsign format" do
      user = %User{callsign: "K1ABC"}

      changeset = User.callsign_changeset(user, %{callsign: "INVALID"})
      errors = errors_on(changeset)
      assert Map.has_key?(errors, :callsign)
      assert "must be a valid amateur radio callsign" in errors.callsign
    end

    test "accepts valid callsign change" do
      user = %User{callsign: "K1ABC"}
      changeset = User.callsign_changeset(user, %{callsign: "W2XYZ"})
      assert changeset.valid?
      assert get_change(changeset, :callsign) == "W2XYZ"
    end
  end
end

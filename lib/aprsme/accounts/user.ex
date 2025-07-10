defmodule Aprsme.Accounts.User do
  @moduledoc false
  use Ecto.Schema

  import Ecto.Changeset

  schema "users" do
    field(:email, :string)
    field(:callsign, :string)
    field(:password, :string, virtual: true, redact: true)
    field(:hashed_password, :string, redact: true)
    field(:confirmed_at, :naive_datetime)

    timestamps()
  end

  @doc """
  A user changeset for registration.

  It is important to validate the length of both email and password.
  Otherwise databases may truncate the email without warnings, which
  could lead to unpredictable or insecure behaviour. Long passwords may
  also be very expensive to hash for certain algorithms.

  ## Options

    * `:hash_password` - Hashes the password so it can be stored securely
      in the database and ensures the password field is cleared to prevent
      leaks in the logs. If password hashing is not needed and clearing the
      password field is not desired (like when using this changeset for
      validations on a LiveView form), this option can be set to `false`.
      Defaults to `true`.

    * `:validate_email` - Validates the uniqueness of the email, in case
      you don't want to validate the uniqueness of the email (like when
      using this changeset for validations on a LiveView form before
      submitting the form), this option can be set to `false`.
      Defaults to `true`.
  """
  def registration_changeset(user, attrs, opts \\ []) do
    user
    |> cast(attrs, [:email, :password, :callsign])
    |> validate_email(opts)
    |> validate_callsign(opts)
    |> validate_password(opts)
  end

  defp validate_email(changeset, opts) do
    changeset
    |> validate_required([:email])
    |> validate_format(:email, ~r/^[^\s]+@[^\s]+$/, message: "must have the @ sign and no spaces")
    |> validate_length(:email, max: 160)
    |> maybe_validate_unique_email(opts)
  end

  defp validate_callsign(changeset, opts) do
    changeset
    |> validate_required([:callsign])
    |> validate_format(:callsign, ~r/^[A-Z]{1,2}[0-9]{1,2}[A-Z]{1,3}$/i,
      message: "must be a valid amateur radio callsign"
    )
    |> update_change(:callsign, &String.upcase/1)
    |> validate_length(:callsign, min: 3, max: 10)
    |> maybe_validate_unique_callsign(opts)
  end

  defp validate_password(changeset, opts) do
    changeset
    |> validate_required([:password])
    |> validate_length(:password, min: 12, max: 72)
    # |> validate_format(:password, ~r/[a-z]/, message: "at least one lower case character")
    # |> validate_format(:password, ~r/[A-Z]/, message: "at least one upper case character")
    # |> validate_format(:password, ~r/[!?@#$%^&*_0-9]/, message: "at least one digit or punctuation character")
    |> maybe_hash_password(opts)
  end

  defp maybe_hash_password(changeset, opts) do
    hash_password? = Keyword.get(opts, :hash_password, true)
    password = get_change(changeset, :password)

    do_hash_password(changeset, hash_password?, password)
  end

  defp do_hash_password(changeset, true, password) when is_binary(password) do
    if changeset.valid? do
      changeset
      # If using Bcrypt, then further validate it is at most 72 bytes long
      |> validate_length(:password, max: 72, count: :bytes)
      |> put_change(:hashed_password, Bcrypt.hash_pwd_salt(password))
      |> delete_change(:password)
    else
      changeset
    end
  end

  defp do_hash_password(changeset, _, _), do: changeset

  defp maybe_validate_unique_email(changeset, opts) do
    validate_email? = Keyword.get(opts, :validate_email, true)
    do_validate_unique_email(changeset, validate_email?)
  end

  defp do_validate_unique_email(changeset, true) do
    changeset
    |> unsafe_validate_unique(:email, Aprsme.Repo)
    |> unique_constraint(:email)
  end

  defp do_validate_unique_email(changeset, false), do: changeset

  defp maybe_validate_unique_callsign(changeset, opts) do
    validate_callsign? = Keyword.get(opts, :validate_callsign, true)
    do_validate_unique_callsign(changeset, validate_callsign?)
  end

  defp do_validate_unique_callsign(changeset, true) do
    changeset
    |> unsafe_validate_unique(:callsign, Aprsme.Repo)
    |> unique_constraint(:callsign)
  end

  defp do_validate_unique_callsign(changeset, false), do: changeset

  @doc """
  A user changeset for changing the email.

  It requires the email to change otherwise an error is added.
  """
  def email_changeset(user, attrs, opts \\ []) do
    user
    |> cast(attrs, [:email])
    |> validate_email(opts)
    |> case do
      %{changes: %{email: _}} = changeset -> changeset
      %{} = changeset -> add_error(changeset, :email, "did not change")
    end
  end

  @doc """
  A user changeset for changing the callsign.

  It requires the callsign to change otherwise an error is added.
  """
  def callsign_changeset(user, attrs, opts \\ []) do
    user
    |> cast(attrs, [:callsign])
    |> validate_callsign(opts)
    |> case do
      %{changes: %{callsign: _}} = changeset -> changeset
      %{} = changeset -> add_error(changeset, :callsign, "did not change")
    end
  end

  @doc """
  A user changeset for changing the password.

  ## Options

    * `:hash_password` - Hashes the password so it can be stored securely
      in the database and ensures the password field is cleared to prevent
      leaks in the logs. If password hashing is not needed and clearing the
      password field is not desired (like when using this changeset for
      validations on a LiveView form), this option can be set to `false`.
      Defaults to `true`.
  """
  def password_changeset(user, attrs, opts \\ []) do
    user
    |> cast(attrs, [:password])
    |> validate_confirmation(:password, message: "does not match password")
    |> validate_password(opts)
  end

  @doc """
  Confirms the account by setting `confirmed_at`.
  """
  def confirm_changeset(user) do
    now = NaiveDateTime.truncate(NaiveDateTime.utc_now(), :second)
    change(user, confirmed_at: now)
  end

  @doc """
  Verifies the password.

  If there is no user or the user doesn't have a password, we call
  `Bcrypt.no_user_verify/0` to avoid timing attacks.
  """
  def valid_password?(%Aprsme.Accounts.User{hashed_password: hashed_password}, password)
      when is_binary(hashed_password) and byte_size(password) > 0 do
    Bcrypt.verify_pass(password, hashed_password)
  end

  def valid_password?(_, _) do
    Bcrypt.no_user_verify()
    false
  end

  @doc """
  Validates the current password otherwise adds an error to the changeset.
  """
  def validate_current_password(changeset, password) do
    if valid_password?(changeset.data, password) do
      changeset
    else
      add_error(changeset, :current_password, "is not valid")
    end
  end
end

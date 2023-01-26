defmodule Aprs.Repo do
  use Ecto.Repo,
    otp_app: :aprs,
    adapter: Ecto.Adapters.Postgres
end

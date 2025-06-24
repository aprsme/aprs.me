defmodule Aprsme.Repo do
  use Ecto.Repo,
    otp_app: :aprsme,
    adapter: Ecto.Adapters.Postgres
end

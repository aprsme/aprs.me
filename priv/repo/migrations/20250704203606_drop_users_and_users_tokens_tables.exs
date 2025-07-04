defmodule Aprsme.Repo.Migrations.DropUsersAndUsersTokensTables do
  use Ecto.Migration

  def change do
    drop table(:users_tokens)
    drop table(:users)
  end
end

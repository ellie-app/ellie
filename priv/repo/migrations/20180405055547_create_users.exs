defmodule Ellie.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def up do
    Ellie.Settings.up
    create table(:users, primary_key: false) do
      add :id, :uuid, primary_key: true
      add :settings, Ellie.Settings.type, null: false
      timestamps()
    end
  end

  def down do
    drop_if_exists table(:users)
    Ellie.Settings.down
  end
end

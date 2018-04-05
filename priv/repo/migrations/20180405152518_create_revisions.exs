defmodule Ellie.Repo.Migrations.CreateRevisions do
  use Ecto.Migration

  def up do
    Ellie.Elm.Version.up
    Ellie.Elm.Package.up

    create table(:revisions, primary_key: false) do
      add :project_id,      :uuid,                            primary_key: true
      add :revision_number, :integer,                         primary_key: true
      add :title,           :string
      add :elm_code,        :text,                            null: false
      add :html_code,       :text,                            null: false
      add :packages,        {:array, Ellie.Elm.Package.type}, null: false
      add :elm_version,     Ellie.Elm.Version.type,           null: false
      add :terms_version,   :integer
      add :user_id,         references(:users, type: :uuid),  null: false
      timestamps(updated_at: false)
    end
  end

  def down do
    drop_if_exists table(:revisions)
    Ellie.Elm.Package.down
    Ellie.Elm.Version.down
  end
end

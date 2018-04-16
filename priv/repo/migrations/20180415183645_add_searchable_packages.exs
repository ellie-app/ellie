defmodule Ellie.Repo.Migrations.AddSearchablePackages do
  use Ecto.Migration

  def up do
    execute "create extension if not exists pg_trgm"

    create table(:searchable_packages, primary_key: false) do
      add :name, Ellie.Elm.Name.type, primary_key: true
      add :summary, :string, null: false
      add :versions, {:array, Ellie.Elm.Version.type}, null: false
      timestamps(updated_at: false)
    end

    create index("searchable_packages", ["summary gin_trgm_ops"],
      name: :searchable_packages_trgm_index_on_summary,
      using: "GIN")

    create index("searchable_packages", ["((name).username) gin_trgm_ops"],
      name: :searchable_packages_trgm_index_on_name_username,
      using: "GIN")

    create index("searchable_packages", ["((name).project) gin_trgm_ops"],
      name: :searchable_packages_trgm_index_on_name_project,
      using: "GIN")
  end

  def down do
    drop_if_exists index("searchable_packages", [:searchable_packages_trgm_index_on_name_project])
    drop_if_exists index("searchable_packages", [:searchable_packages_trgm_index_on_name_username])
    drop_if_exists index("searchable_packages", [:searchable_packages_trgm_index_on_summary])
    drop_if_exists table("searchable_packages")
    execute "drop extension if exists pg_trgm"
  end
end

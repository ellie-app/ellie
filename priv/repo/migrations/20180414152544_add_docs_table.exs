defmodule Ellie.Repo.Migrations.AddDocsTable do
  use Ecto.Migration

  def up do
    create table(:package_docs, primary_key: false) do
      add :package, Ellie.Elm.Package.type, primary_key: true
      add :docs_0_19_0, {:array, :map}
      timestamps()
    end
  end

  def down do
    drop_if_exists table(:package_docs)
  end
end

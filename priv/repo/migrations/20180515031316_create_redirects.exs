defmodule Ellie.Repo.Migrations.CreateRedirects do
  use Ecto.Migration
  alias Ellie.Types.PrettyId

  def up do
    create table(:redirects, primary_key: false) do
      add :project_id, PrettyId.type, primary_key: true
      add :revision_number, :integer, primary_key: true
      add :revision_id, references(:revisions, type: PrettyId.type)
    end
  end

  def down do
    drop_if_exists table(:redirects)
  end
end

defmodule Ellie.Types.Redirect do
  alias Ellie.Types.Revision
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key false
  schema "redirects" do
    field :project_id, :integer, primary_key: true, read_after_writes: true
    field :revision_number, :integer, primary_key: true, default: 0, read_after_writes: true
    belongs_to :revision, Revision
  end

  @doc false
  def changeset(revision, attrs) do
    revision
    |> cast(attrs, [:project_id, :revision_number, :revision_id])
    |> validate_required([:project_id, :revision_number, :revision_id])
  end
end

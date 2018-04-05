defmodule Ellie.Revision do
  use Ecto.Schema
  import Ecto.Changeset


  @primary_key false
  schema "revisions" do
    field :project_id, :binary_id, primary_key: true, autogenerate: true
    field :revision_number, :integer, primary_key: true, default: 0
    field :title, :string
    field :elm_code, :string
    field :html_code, :string
    field :packages, {:array, Ellie.Elm.Package}
    field :elm_version, Ellie.Elm.Version
    field :terms_version, :integer
    belongs_to :user, Ellie.User, foreign_key: :user_id, type: :binary_id
    timestamps(updated_at: false)
  end

  @doc false
  def changeset(revision, attrs) do
    revision
    |> cast(attrs, [:project_id, :revision_number, :title, :elm_code, :html_code, :packages, :elm_version, :terms_version])
    |> validate_required([:project_id, :revision_id, :elm_code, :html_code, :packages, :elm_version, :user])
  end
end

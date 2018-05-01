defmodule Ellie.Types.Revision do
  alias Ellie.Types.User
  alias Elm.Ecto.Package
  alias Elm.Ecto.Version
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key false
  schema "revisions" do
    field :project_id, :integer, primary_key: true, read_after_writes: true
    field :revision_number, :integer, primary_key: true, default: 0, read_after_writes: true
    field :title, :string
    field :elm_code, :string
    field :html_code, :string
    field :packages, {:array, Package}
    field :elm_version, Version
    field :terms_version, :integer
    belongs_to :user, User, foreign_key: :user_id, type: :binary_id
    timestamps(updated_at: false)
  end

  @doc false
  def changeset(revision, attrs) do
    revision
    |> cast(attrs, [:project_id, :revision_number, :title, :elm_code, :html_code, :packages, :elm_version, :terms_version])
    |> validate_required([:project_id, :revision_id, :elm_code, :html_code, :packages, :elm_version, :user])
  end
end

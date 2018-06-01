defmodule Ellie.Types.Revision do
  alias Elm.Ecto.Package
  alias Elm.Ecto.Version
  alias Ellie.Types.PrettyId
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, PrettyId, autogenerate: false, read_after_writes: true}
  schema "revisions" do
    field :title, :string
    field :elm_code, :string
    field :html_code, :string
    field :packages, {:array, Package}
    field :elm_version, Version
    field :terms_version, :integer
    timestamps(updated_at: false)
  end

  @doc false
  def changeset(revision, attrs) do
    revision
    |> cast(attrs, [:id, :title, :elm_code, :html_code, :packages, :elm_version, :terms_version])
    |> validate_required([:id, :elm_code, :html_code, :packages, :elm_version, :terms_version])
  end
end

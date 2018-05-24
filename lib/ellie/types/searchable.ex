defmodule Ellie.Types.Searchable do
  alias Elm.Ecto.Name
  alias Elm.Ecto.Version
  import Ecto.Changeset
  use Ecto.Schema

  @primary_key false
  schema "searchable_packages" do
    field :name, Name, primary_key: true
    field :summary, :string
    field :versions, {:array, Version}
    field :score, :float, virtual: true
    timestamps(updated_at: false)
  end

  @doc false
  def changeset(revision, attrs) do
    revision
    |> cast(attrs, [:name, :summary, :versions])
    |> validate_required([:name, :summary, :versions])
  end

  def to_package(%__MODULE__{name: name, versions: versions}) do
    %Elm.Package{
      name: name,
      version: Enum.max(versions)
    }
  end
end

defmodule Ellie.Elm.Docs do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key false
  schema "package_docs" do
    field :package, Ellie.Elm.Package, primary_key: true
    field :docs_0_19_0, {:array, :map}
    timestamps()
  end

  @doc false
  def changeset(revision, attrs) do
    revision
    |> cast(attrs, [:package, :docs_0_19_0])
    |> validate_required([:package])
  end

  def data() do
    EllieWeb.Graphql.Loaders.Ecto.new(Ellie.Repo, query: &query/2)
  end

  def query(queryable, _params) do
    queryable
  end
end

defmodule Ellie.Elm.Searchable do
  use    Ecto.Schema
  import Ecto.Changeset
  alias  Ellie.Elm.Name
  alias  Ellie.Elm.Version
  alias  Ellie.Helpers.EnumHelpers
  alias  Ellie.Elm.Package

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

  def from_json(map) do
    with {:ok, name} <- Name.from_string(Map.get(map, "name", "")),
      summary when not is_nil(summary) <- Map.get(map, "summary"),
      version_list when is_list(version_list) <- Map.get(map, "versions"),
      {:ok, versions} <- EnumHelpers.traverse_result(version_list, &Version.from_string/1)
    do
      {:ok, %Ellie.Elm.Searchable{name: name, summary: summary, versions: versions}}
    else
      _ -> {:error, "could not parse searchanble"}
    end
  end

  def to_map(%Ellie.Elm.Searchable{name: name, summary: summary, versions: versions}) do
    %{name: name, summary: summary, versions: versions, inserted_at: DateTime.utc_now()}
  end

  def to_package(%Ellie.Elm.Searchable{name: name, versions: versions}) do
    %Package{
      name: name,
      version: Enum.max(versions, &make_version/0)
    }
  end

  defp make_version() do
    %Version{}
  end
end

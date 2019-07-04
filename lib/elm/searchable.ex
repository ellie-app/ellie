defmodule Elm.Searchable do
  alias Elm.Name
  alias Elm.Version
  alias Ellie.Helpers.EnumHelpers
  alias Elm.Package

  defstruct [:name, :summary, :versions]

  @type t :: %Elm.Searchable{name: Name.t(), summary: String.t(), versions: list(Version.t())}

  def from_json(map) do
    with {:ok, name} <- Name.from_string(Map.get(map, "name", "")),
         summary when not is_nil(summary) <- Map.get(map, "summary"),
         version_list when is_list(version_list) <- Map.get(map, "versions"),
         {:ok, versions} <- EnumHelpers.traverse_result(version_list, &Version.from_string/1) do
      {:ok, %Elm.Searchable{name: name, summary: summary, versions: versions}}
    else
      _ -> {:error, "could not parse searchanble"}
    end
  end

  def to_map(%Elm.Searchable{name: name, summary: summary, versions: versions}) do
    %{name: name, summary: summary, versions: versions, inserted_at: DateTime.utc_now()}
  end

  def to_package(%Elm.Searchable{name: name, versions: versions}) do
    %Package{
      name: name,
      version: Enum.max(versions, &make_version/0)
    }
  end

  defp make_version() do
    %Version{}
  end
end

defmodule Elm.Project do
  alias Elm.Version
  alias Elm.Package

  defstruct [:elm_version, :dependencies]

  @type t :: %Elm.Project{
    elm_version: Version.t,
    dependencies: MapSet.t(Package.t)
  }

  # def to_json(%Ellie.Elm.Project{} = project) do
  #   %{
  #     "type": "application",
  #     "source-directories": project.source_dirs,
  #     "elm-version": Version.to_string(project.elm_version),
  #     "dependencies": Enum.reduce(project.deps, %{}, &deps_to_json/2),
  #     "test-dependencies": %{},
  #     "do-not-edit-this-by-hand": %{
  #       "transitive-dependencies": Enum.reduce(project.trans_deps, %{}, &dep_from_json/2)
  #     }
  #   }
  # end

  # defp deps_to_json(p, map) do
  #   Map.put(map, Name.to_string(p.name), Version.to_string(p.version))
  # end

  # def from_json(value) do
  #   with source_dirs <- Map.get(value, "source-directories", []),
  #     {:ok, elm_version} <- value |> Map.get("elm-version", "0.19.0") |> Version.from_string(),
  #     deps <- Map.get(value, "dependencies", %{}),
  #     trans_deps <- value |> Map.get("do-not-edit-this-by-hand", %{}) |> Map.get("transitive-dependencies", %{})
  #   do
  #     {:ok, %Ellie.Elm.Project{
  #       source_dirs: source_dirs,
  #       elm_version: elm_version,
  #       deps: Enum.reduce(deps, MapSet.new(), &dep_from_json/2),
  #       trans_deps: Enum.reduce(trans_deps, MapSet.new(), &dep_from_json/2)
  #     }}
  #   else
  #     e -> e
  #   end
  # end

  # defp dep_from_json({k, v}, set) do
  #   with {:ok, name} <- Name.from_string(k),
  #     {:ok, version} <- Version.from_string(v)
  #   do
  #     MapSet.put(set, %Package{name: name, version: version})
  #   else
  #     _ -> set
  #   end
  # end
end

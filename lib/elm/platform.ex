defmodule Elm.Platform do
  alias Elm.Package
  alias Elm.Project
  alias Elm.Error
  alias Elm.Docs
  alias Elm.Version
  alias Elm.Searchable

  @callback setup(Path.t, Version.t) :: {:ok, Project.t} | :error
  @callback format(String.t, Version.t) :: {:ok, String.t} | :error
  @callback compile(Path.t, [source: String.t, project: Project.t, output: Path.t]) :: {:ok, Error.t | nil} | :error
  @callback docs(Package.t) :: {:ok, list(Docs.Module.t)} | :error
  @callback search() :: {:ok, list(Searchable.t)} | :error

  @latest_version Version.create(0, 19, 0)

  @spec latest_version() :: Version.t
  def latest_version() do
    @latest_version
  end

  @spec setup(root :: Path.t, version :: Version.t) :: {:ok, Project.t} | :error
  def setup(root, version) do
    adapter().setup(root, version)
  end

  @spec format(code :: String.t, version :: Version.t) :: {:ok, String.t} | :error
  def format(code, version) do
    adapter().format(code, version)
  end

  @spec compile(Path.t, [source: String.t, project: Project.t, output: Path.t]) :: {:ok, Error.t | nil} | :error
  def compile(root, options) do
    adapter().compile(root, options)
  end

  @spec docs(Package.t) :: {:ok, list(Docs.Module)} | :error
  def docs(package) do
    adapter().docs(package)
  end

  @spec search() :: {:ok, list(Searchable.t)} | :error
  def search() do
    adapter().search()
  end

  defp adapter() do
    Application.get_env(:ellie, Elm, [])
    |> Keyword.get(:platform_adapter, Elm.Platform.Local)
  end
end

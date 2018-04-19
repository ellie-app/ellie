defmodule Ellie.Elm.Platform do
  alias Ellie.Elm.Package
  alias Ellie.Elm.Version
  alias Ellie.Elm.Platform.Parser

  @callback setup(Path.t) :: :ok | {:error, String.t}
  @callback dependencies(Path.t) :: {:ok, MapSet.t(Package.t)}
  @callback compile([root: Path.t, entry: Path.t, output: Path.t]) :: {:ok, map | nil} | {:error, String.t}
  @callback format(String.t) :: {:ok, String.t} | {:error, String.t}
  @callback install(Path.t, MapSet.t(Package.t)) :: :ok


  @spec setup!(Version.t, Path.t) :: :ok | {:error, String.t}
  def setup!(version, root) do
    File.mkdir_p!(version_root(version, root))
    get_impl!(version).setup(version_root(version, root))
  end

  @spec dependencies!(Version.t, Path.t) :: {:ok, MapSet.t(Package.t)}
  def dependencies!(version, root) do
    get_impl!(version).dependencies(version_root(version, root))
  end

  @spec compile!(Version.t, Path.t, String.t) :: {:ok, map | nil} | {:error, String.t}
  def compile!(version, root, code) do
    src_path = Path.join(version_root(version, root), "src")
    module_path = Path.join("src", Parser.module_path(code))
    full_module_path = Path.join([root, Version.to_string(version), module_path])
    File.rm_rf!(src_path)
    File.mkdir_p!(Path.dirname(full_module_path))
    File.write!(full_module_path, code)
    get_impl!(version).compile(root: version_root(version, root), entry: module_path, output: "build.js")
  end

  def result!(version, root) do
    Path.join(version_root(version, root), "build.js")
  end

  @spec format!(Version.t, String.t) :: {:ok, String.t} | {:error, String.t}
  def format!(version, code) do
    get_impl!(version).format(code)
  end

  @spec install!(Version.t, Path.t, MapSet.t(Package.t)) :: :ok
  def install!(version, root, packages) do
    get_impl!(version).install(version_root(version, root), packages)
  end

  defp get_impl!(version) do
    case version do
      %Version{major: 0, minor: 19, patch: 0} -> Ellie.Elm.Platform.Impl19
      %Version{major: 0, minor: 18, patch: 0} -> Ellie.Elm.Platform.Impl18
      _ -> raise ArgumentError, "No suppored Elm version #{Version.to_string(version)}"
    end
  end

  defp version_root(version, root) do
    Path.join(root, Version.to_string(version))
  end
end

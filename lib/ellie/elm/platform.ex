defmodule Ellie.Elm.Platform do
  alias Ellie.Elm.Package
  alias Ellie.Elm.Version

  @callback setup(Path.t) :: :ok | {:error, String.t}
  @callback dependencies(Path.t) :: {:ok, MapSet.t(Package.t)}
  @callback compile([root: Path.t, entry: Path.t, output: Path.t]) :: {:ok, map | nil} | {:error, String.t}
  @callback format(String.t) :: {:ok, String.t} | {:error, String.t}
  @callback install(Path.t, MapSet.t(Package.t)) :: :ok


  @spec setup!(Version.t, Path.t) :: :ok | {:error, String.t}
  def setup!(version, root) do
    get_impl!(version).setup(root)
  end

  @spec dependencies!(Version.t, Path.t) :: {:ok, MapSet.t(Package.t)}
  def dependencies!(version, root) do
    get_impl!(version).dependencies(root)
  end

  @spec compile!(Version.t, [root: Path.t, entry: Path.t, output: Path.t]) :: {:ok, map | nil} | {:error, String.t}
  def compile!(version, options) do
    get_impl!(version).compile(options)
  end

  @spec format!(Version.t, String.t) :: {:ok, String.t} | {:error, String.t}
  def format!(version, code) do
    get_impl!(version).format(code)
  end

  @spec install!(Version.t, Path.t, MapSet.t(Package.t)) :: :ok
  def install!(version, root, packages) do
    get_impl!(version).install(root, packages)
  end

  defp get_impl!(version) do
    case version do
      %Version{major: 0, minor: 19, patch: 0} -> Ellie.Elm.Platform.Impl19
      %Version{major: 0, minor: 18, patch: 0} -> Ellie.Elm.Platform.Impl18
      _ -> raise ArgumentError, "No suppored Elm version #{Version.to_string(version)}"
    end
  end
end

defmodule Ellie.Domain.Workspace do
  alias Ellie.Types.User
  alias Elm.Version

  @callback dependencies(user :: User.t, version :: Version.t) :: {:ok, MapSet.t(Package.t)} | :error
  @callback compile(user :: User.t, version :: Version.t, elm_code :: String.t, packages :: MapSet.t(Package.t)) :: {:ok, Error.t | nil} | :error
  @callback result(user :: User.t) :: {:ok, {Path.t, String.t}} | :error
  @callback cleanup_after(user :: User.t, process :: pid) :: :unit

  @spec dependencies(user :: User.t, version :: Version.t) :: {:ok, MapSet.t(Package.t)} | :error
  def dependencies(user, version) do
    adapter().dependencies(user, version)
  end

  @spec compile(user :: User.t, version :: Version.t, elm_code :: String.t, packages :: list(Package.t)) :: {:ok, Error.t | nil} | :error
  def compile(user, version, elm_code, packages) do
    adapter().compile(user, version, elm_code, packages)
  end

  @spec result(user :: User.t) :: {:ok, {Path.t, String.t}} | :error
  def result(user) do
    adapter().result(user)
  end

  @spec cleanup_after(user :: User.t, process :: pid) :: :unit
  def cleanup_after(user, process) do
    adapter().cleanup_after(user, process)
  end

  defp adapter() do
    config = Application.get_env(:ellie, Ellie.Domain.Workspace, [])
    Keyword.get(config, :adapter, Ellie.Adapters.Workspace.Local)
  end
end

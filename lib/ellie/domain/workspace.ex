defmodule Ellie.Domain.Workspace do
  alias Elm.Version
  alias Data.Uuid

  @callback create() :: {:ok, Uuid.t()} | :error
  @callback watch(id :: Uuid.t(), process :: pid) :: :unit
  @callback dependencies(id :: Uuid.t(), version :: Version.t()) ::
              {:ok, MapSet.t(Package.t())} | :error
  @callback compile(
              id :: Uuid.t(),
              version :: Version.t(),
              elm_code :: String.t(),
              packages :: MapSet.t(Package.t())
            ) :: {:ok, Error.t() | nil} | :error
  @callback result(id :: Uuid.t()) :: {:ok, {Path.t(), String.t()}} | :error
  @callback cleanup() :: :unit

  @spec create() :: {:ok, Uuid.t()} | :error
  def create() do
    adapter().create()
  end

  @spec watch(id :: Uuid.t(), process :: pid) :: :unit
  def watch(id, process) do
    adapter().watch(id, process)
  end

  @spec cleanup() :: :unit
  def cleanup() do
    adapter().cleanup()
  end

  @spec dependencies(id :: Uuid.t(), version :: Version.t()) ::
          {:ok, MapSet.t(Package.t())} | :error
  def dependencies(id, version) do
    adapter().dependencies(id, version)
  end

  @spec compile(
          id :: Uuid.t(),
          version :: Version.t(),
          elm_code :: String.t(),
          packages :: list(Package.t())
        ) :: {:ok, Error.t() | nil} | :error
  def compile(id, version, elm_code, packages) do
    adapter().compile(id, version, elm_code, packages)
  end

  @spec result(id :: Uuid.t()) :: {:ok, {Path.t(), String.t()}} | :error
  def result(id) do
    adapter().result(id)
  end

  defp adapter() do
    config = Application.get_env(:ellie, Ellie.Domain.Workspace, [])
    Keyword.get(config, :adapter, Ellie.Adapters.Workspace.Local)
  end
end

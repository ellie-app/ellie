defmodule Ellie.Domain.Search do
  alias Elm.Package

  @callback search(query :: String.t) :: list(Package.t)
  @callback reload() :: :ok | :error

  @spec search(query :: String.t) :: list(Package.t)
  def search(query) do
    adapter().search(query)
  end

  @spec reload() :: :ok | :error
  def reload() do
    adapter().reload()
  end

  defp adapter() do
    config = Application.get_env(:ellie, Ellie.Domain.Search, [])
    Keyword.get(config, :adapter, Ellie.Adapters.Search.Ecto)
  end
end

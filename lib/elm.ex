defmodule Elm do
  def start_link() do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    import Supervisor.Spec

    children = [
      worker(Elm.Platform.Local18, [])
    ]
    supervise(children, strategy: :one_for_one)
  end
end

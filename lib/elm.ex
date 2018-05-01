defmodule Elm do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      worker(Elm.Platform.Local18, [])
    ]

    opts = [strategy: :one_for_one, name: Elm.Supervisor]
    Supervisor.start_link(children, opts)
  end
end

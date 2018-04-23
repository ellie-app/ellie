defmodule Ellie.Application do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      supervisor(Ellie.Repo, []),
      supervisor(EllieWeb.Endpoint, []),
      supervisor(Absinthe.Subscription, [EllieWeb.Endpoint]),
      worker(Ellie.Workspace, []),
      worker(Ellie.Elm.Platform.Impl18, []),
      worker(Ellie.Embed, []),
      worker(Ellie.Scheduler, [])
    ]

    opts = [strategy: :one_for_one, name: Ellie.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def config_change(changed, _new, removed) do
    EllieWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
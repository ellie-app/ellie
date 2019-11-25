defmodule Ellie do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      supervisor(Elm, []),
      supervisor(Ellie.Repo, []),
      supervisor(EllieWeb.Endpoint, []),
      supervisor(Absinthe.Subscription, [EllieWeb.Endpoint]),
      worker(Ellie.Adapters.Embed.Local, []),
      worker(Ellie.Adapters.Workspace.Local, []),
      worker(EllieWeb.Scheduler, []),
      worker(Task, [&Ellie.Domain.Search.reload/0], restart: :temporary)
    ]

    {:ok, _} = Logger.add_backend(Sentry.LoggerBackend)

    opts = [strategy: :one_for_one, name: Ellie.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def config_change(changed, _new, removed) do
    EllieWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end

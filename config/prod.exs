use Mix.Config

config :logger, backends: []

config :ellie, EllieWeb.Endpoint,
  load_from_system_env: true,
  cache_static_manifest: "priv/static/manifest.json",
  http: [port: {:system, "PORT"}],
  url: [host: "localhost", port: {:system, "PORT"}],
  server: true,
  root: ".",
  version: Application.spec(:phoenix_distillery, :vsn)

config :ellie, Ellie.Repo,
  adapter: Ecto.Adapters.Postgres,
  ssl: true,
  pool_size: 5

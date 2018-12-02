use Mix.Config

config :logger, backends: []

config :ellie, EllieWeb.Endpoint,
  force_ssl: [rewrite_on: [:x_forwarded_proto]],
  cache_static_manifest: "priv/static/manifest.json",
  server: true,
  version: Application.spec(:ellie, :vsn)

config :ellie, Ellie.Repo,
  adapter: Ecto.Adapters.Postgres,
  ssl: true,
  pool_size: 5

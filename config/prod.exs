use Mix.Config

config :logger, backends: []

config :ellie, EllieWeb.Endpoint,
  load_from_system_env: true,
  cache_static_manifest: "priv/static/manifest.json",
  check_origin: ["https://ellie-app.com"]

config :ellie, Ellie.Repo,
  adapter: Ecto.Adapters.Postgres,
  ssl: true,
  pool_size: 5

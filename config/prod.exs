use Mix.Config

config :logger, level: :info

config :ellie, EllieWeb.Endpoint,
  load_from_system_env: true,
  cache_static_manifest: "priv/static/cache_manifest.json"

config :ellie, Ellie.Repo,
  adapter: Ecto.Adapters.Postgres,
  ssl: true,
  pool_size: 10

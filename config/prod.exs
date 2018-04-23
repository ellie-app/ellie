use Mix.Config

config :logger, level: :info

config :ellie, EllieWeb.Endpoint,
  load_from_system_env: true,
  server: true,
  secret_key_base: "${SECRET_KEY_BASE}",
  url: [host: "menacing-portly-alaskankleekai.gigalixirapp.com", port: 80],
  cache_static_manifest: "priv/static/cache_manifest.json"

config :ellie, Ellie.Repo,
  adapter: Ecto.Adapters.Postgres,
  url: "${DATABASE_URL}",
  database: "",
  ssl: true,
  pool_size: 1 # Free tier db only allows 1 connection

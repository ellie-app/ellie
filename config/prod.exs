use Mix.Config

config :logger, level: :info

config :ellie, EllieWeb.Endpoint,
  load_from_system_env: true,
  secret_key_base: Map.fetch!(System.get_env(), "SECRET_KEY_BASE"),
  cache_static_manifest: "priv/static/cache_manifest.json",
  url: [scheme: "https", host: System.get_env("SERVER_HOST"), port: 443],
  force_ssl: [rewrite_on: [:x_forwarded_proto]]

config :ellie, Ellie.Repo,
  adapter: Ecto.Adapters.Postgres,
  url: System.get_env("DATABASE_URL"),
  database: "",
  ssl: true,
  pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10")

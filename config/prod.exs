use Mix.Config

config :logger,
  level: :info,
  handle_sasl_reports: true,
  handle_otp_reports: true

config :ellie, EllieWeb.Endpoint,
  cache_static_manifest: "priv/static/manifest.json",
  server: true,
  version: Application.spec(:ellie, :vsn)

config :ellie, Ellie.Repo,
  adapter: Ecto.Adapters.Postgres,
  ssl: true,
  pool_size: 5

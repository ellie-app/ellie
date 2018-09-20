use Mix.Config

config :porcelain, :goon_driver_path, "#{:code.priv_dir(:ellie)}/bin/goon"
config :porcelain, driver: Porcelain.Driver.Goon

config :ellie, EllieWeb.Endpoint,
  http: [port: 4000],
  url: [host: "localhost", port: 4000],
  # TODO: use ["ellie-app.com"]
  check_origin: false,
  root: ".",
  secret_key_base: System.get_env("SECRET_KEY_BASE")

config :ellie, Ellie.Repo,
  url: System.get_env("DATABASE_URL")

config :sentry,
  dsn: System.get_env("SENTRY_DSN"),
  api_key: System.get_env("SENTRY_API_KEY")

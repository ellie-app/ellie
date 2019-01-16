use Mix.Config

config :porcelain, :goon_driver_path, "#{:code.priv_dir(:ellie)}/bin/goon"
config :porcelain, driver: Porcelain.Driver.Goon

config :ellie, EllieWeb.Endpoint,
  http: [port: 4000],
  url: [host: "localhost", port: 4000],
  check_origin: false,
  root: ".",
  secret_key_base: System.get_env("SECRET_KEY_BASE")

config :ellie, Ellie.Repo, url: System.get_env("DATABASE_URL")

config :sentry,
  dsn: nil,
  api_key: nil

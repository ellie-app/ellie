use Mix.Config

config :porcelain, :goon_driver_path, Path.join(:code.priv_dir(:ellie), "bin/goon")
config :porcelain, driver: Porcelain.Driver.Goon

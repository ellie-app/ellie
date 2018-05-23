# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :ellie,
  ecto_repos: [Ellie.Repo],
  env: Mix.env

config :ellie, Ellie.Scheduler,
  global: true,
  jobs: [
    {"*/15 * * * *", {Ellie.Jobs.ReloadSearch, :run, []}},
    {"@daily", {Ellie.Jobs.CleanupLogs, :run, []}}
  ]

config :phoenix, :template_engines,
  md: PhoenixMarkdown.Engine

# Configures the endpoint
config :ellie, EllieWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "+ODF8PyQMpBDb5mxA117MqkLne/bGi0PZoTl5uIHAzck2hDAJ8uGJPzark0Aolyi",
  render_errors: [view: EllieWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Ellie.PubSub,
           adapter: Phoenix.PubSub.PG2]

config :absinthe,
  schema: EllieWeb.Graphql.Schema

config :porcelain, :goon_driver_path, Path.expand("../priv/bin/goon", __DIR__)
config :porcelain, driver: Porcelain.Driver.Goon

config :sentry,
  included_environments: [:prod],
  environment_name: Mix.env

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"

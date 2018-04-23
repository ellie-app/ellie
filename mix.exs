defmodule Ellie.Mixfile do
  use Mix.Project

  def project do
    [
      app: :ellie,
      version: "0.0.1",
      elixir: "~> 1.4",
      elixirc_paths: elixirc_paths(Mix.env),
      compilers: [:phoenix] ++ Mix.compilers,
      start_permanent: Mix.env == :prod,
      aliases: aliases(),
      deps: deps(),
      default_task: "phx.server"
    ]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [
      mod: {Ellie.Application, []},
      extra_applications: [:logger, :runtime_tools, :porcelain]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_),     do: ["lib"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:absinthe, "~> 1.4.0"},
      {:absinthe_plug, "~> 1.4.0"},
      {:absinthe_phoenix, "~> 1.4.0"},
      {:briefly, "~> 0.3"},
      {:combine, "~> 0.10"},
      {:cowboy, "~> 1.0"},
      {:dataloader, "~> 1.0.0"},
      {:distillery, "~> 1.5.2"},
      {:download, "~> 0.0.4"},
      {:httpoison, "~> 1.1"},
      {:msgpax, "~> 2.0"},
      {:murmur, "~> 1.0"},
      {:phoenix, "~> 1.3.2"},
      {:phoenix_pubsub, "~> 1.0"},
      {:phoenix_ecto, "~> 3.2"},
      {:postgrex, ">= 0.0.0"},
      {:phoenix_html, "~> 2.10.0"},
      {:phoenix_live_reload, "~> 1.0", only: :dev},
      {:poison, "~> 3.1"},
      {:porcelain, "~> 2.0.3"},
      {:quantum, "~> 2.2"},
      {:timex, "~> 3.0"}
    ]
  end

  # Aliases are shortcuts or tasks specific to the current project.
  # For example, to create, migrate and run the seeds file at once:
  #
  #     $ mix ecto.setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases do
    [
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      "test": ["ecto.create --quiet", "ecto.migrate", "test"],
      "ellie.graphql": ["loadpaths", "absinthe.schema.json ./priv/graphql/schema.json", "ellie.graphqelm"]
    ]
  end
end
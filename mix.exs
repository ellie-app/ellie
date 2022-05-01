defmodule Ellie.Mixfile do
  use Mix.Project

  def project do
    [
      app: :ellie,
      version: "0.0.1",
      elixir: "1.13.4",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [:phoenix] ++ Mix.compilers(),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
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
      mod: {Ellie, []},
      extra_applications: [:logger, :runtime_tools, :porcelain, :sentry]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:absinthe, "1.7.0"},
      {:absinthe_plug, "1.5.8"},
      {:absinthe_phoenix, "2.0.2"},
      {:combine, "~> 0.10"},
      {:cowboy, "~> 2.4"},
      {:dataloader, "~> 1.0.0"},
      {:distillery, "~> 2.0.10"},
      {:httpoison, "~> 1.1"},
      {:murmur, "~> 1.0"},
      {:phoenix, "~> 1.6.7"},
      {:phoenix_pubsub, "~> 2.1.1"},
      {:phoenix_ecto, "~> 4.4.0"},
      {:reverse_proxy_plug, "~> 2.1"},
      {:ecto_sql, "~> 3.0"},
      {:postgrex, ">= 0.0.0"},
      {:phoenix_html, "~> 3.0.0"},
      {:phoenix_live_reload, "~> 1.0", only: :dev},
      {:phoenix_markdown, "~> 1.0"},
      {:jason, "~> 1.0"},
      {:porcelain, "~> 2.0.3"},
      {:quantum, "~> 2.2"},
      {:timex, "~> 3.0"},
      {:sweet_xml, "~> 0.6"},
      {:sentry, "~> 7.0.6"}
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
      test: ["ecto.create --quiet", "ecto.migrate", "test"],
      generate_schema: ["absinthe.schema.json ./priv/graphql/schema.json"]
    ]
  end
end

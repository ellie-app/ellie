defmodule Mix.Tasks.Ellie.Graphqelm do
  use Mix.Task

  def run(_) do
    Mix.Shell.cmd "cd assets && npx graphqelm --introspection-file ../priv/graphql/schema.json --base Ellie.Api --output elm-stuff/generated/dillonkearns/graphqelm", [], fn a -> a end
  end
end

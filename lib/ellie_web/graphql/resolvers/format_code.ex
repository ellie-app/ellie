defmodule EllieWeb.Graphql.Resolvers.FormatCode do
  alias Ellie.Elm.Compiler
  alias Absinthe.Resolution.Helpers

  def call(args, _context) do
    Helpers.async(fn ->
      case Compiler.format(args[:code]) do
        {:ok, code} -> {:ok, code}
        {:error, _} -> {:error, "elm-format failed"}
      end
    end)
  end
end

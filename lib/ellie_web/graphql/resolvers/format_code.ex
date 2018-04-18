defmodule EllieWeb.Graphql.Resolvers.FormatCode do
  alias Ellie.Elm.Platform
  alias Absinthe.Resolution.Helpers

  def call(args, _context) do
    Helpers.async(fn ->
      case Platform.format!(args.elm_version, args.code) do
        {:ok, code} -> {:ok, code}
        {:error, _} -> {:error, "elm-format failed"}
      end
    end)
  end
end

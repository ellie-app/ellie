defmodule EllieWeb.Graphql.Resolvers.SearchPackages do
  def call(args, _context) do
    query = Map.get(args, :query, "")
    if String.length(query) < 3 do
      {:ok, []}
    else
      {:ok, Ellie.Search.search(query)}
    end
  end
end

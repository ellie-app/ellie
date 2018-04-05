defmodule EllieWeb.Graphql.Resolvers.Revision do
  alias Ellie.{Repo, Revision}

  def call(args, _context) do
    {:ok, Repo.get_by(Revision, args)}
  end
end

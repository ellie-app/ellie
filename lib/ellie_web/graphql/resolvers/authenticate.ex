defmodule EllieWeb.Graphql.Resolvers.Authenticate do
  alias EllieWeb.Auth
  alias Ellie.{Repo, User, Settings}

  def call(_args, %{context: %{current_user: current_user}}) do
    {:ok, %{user: current_user, token: Auth.sign(current_user), terms_version: 1}}
  end

  def call(_args, _context) do
    {:ok, user} = Repo.insert(%User{settings: %Settings{}})
    {:ok, %{user: user, token: Auth.sign(user), terms_version: 1}}
  end
end

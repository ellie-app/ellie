defmodule EllieWeb.Graphql.Resolvers.AcceptTerms do
  alias Ellie.Repo

  def call(%{terms: terms}, %{context: %{current_user: current_user}}) do
    changeset = Ecto.Changeset.change(current_user, terms_version: terms)
    case Repo.update(changeset) do
      {:ok, _} -> {:ok, true}
      _ -> {:error, "Failed to accept terms"}
    end
  end

  def call(_args, _context) do
    {:error, "Not authenticated, or no terms version given"}
  end
end

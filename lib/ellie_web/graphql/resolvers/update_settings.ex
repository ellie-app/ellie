defmodule EllieWeb.Graphql.Resolvers.UpdateSettings do
  alias Ellie.Repo

  def call(args, %{context: %{current_user: current_user}}) do
    settings_update = Enum.filter(args, fn {_,v} -> not is_nil(v) end)
    changeset = Ecto.Changeset.change(current_user, settings: Kernel.struct(current_user.settings, settings_update))
    case Repo.update(changeset) do
      {:ok, _} -> {:ok, true}
      _ -> {:error, "Failed to update settings"}
    end
  end

  def call(_args, _context) do
    {:error, "Not authenticated, or no terms version given"}
  end
end

defmodule EllieWeb.Graphql.Resolvers.UpdateRevision do
  alias Ellie.Elm.{Version, Package}
  alias Ellie.{Repo, Revision}
  import Ecto.Query

  def call(%{project_id: project_id, inputs: data}, %{context: %{current_user: current_user}}) do
    case current_user.terms_version do
      nil ->
        {:error, "Cannot save revision without accepting terms of service"}
      terms_version ->
        latest_revision_number =
          Revision
          |> where([r], r.project_id == ^project_id)
          |> select([r], max(r.revision_number))
          |> Repo.one()

        if is_nil(latest_revision_number) do
          {:error, "Revision does not exist and cannot be updated"}
        else
          Repo.insert(%Revision{
            project_id: project_id,
            revision_number: latest_revision_number + 1,
            title: Map.get(data, "title"),
            elm_code: data.elm_code,
            html_code: data.html_code,
            packages: Enum.map(data.packages, &Kernel.struct!(Package, &1)),
            terms_version: terms_version,
            elm_version: %Version{ major: 0, minor: 19, patch: 0 },
            user: current_user
          })
        end
    end
  end

  def call(_, _) do
    {:error, "Bad createRevision call"}
  end
end

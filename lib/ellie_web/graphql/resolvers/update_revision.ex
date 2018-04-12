defmodule EllieWeb.Graphql.Resolvers.UpdateRevision do
  alias Ellie.Elm.{Version, Package}
  alias Ellie.{Repo, Revision}
  import Ecto.Query

  def call(%{project_id: project_id, inputs: data}, %{context: %{current_user: current_user}}) do
    case current_user.terms_version do
      nil ->
        {:error, "Cannot save revision without accepting terms of service"}
      terms_version ->
        {:ok, latest_revision_number} =
          Revision
          |> where([r], r.project_id == ^project_id)
          |> select([r], max(r.revision_numer))
          |> Repo.one()

        Repo.insert(%Revision{
          revision_number: latest_revision_number + 1,
          title: data.title,
          elm_code: data.elm_code,
          html_code: data.html_code,
          packages: Enum.map(data.packages, &Kernel.struct!(Package, &1)),
          terms_version: terms_version,
          elm_version: %Version{ major: 0, minor: 19, patch: 0 },
          user: current_user
        })
    end
  end

  def call(_, _) do
    {:error, "Bad createRevision call"}
  end
end

defmodule Ellie.Adapters.Api.Ecto do
  alias Ellie.Repo
  alias Elm.Version
  alias Elm.Package
  alias Elm.Name
  alias Elm.Platform
  alias Ellie.Helpers.EnumHelpers
  alias Ellie.Types.Revision
  alias Ellie.Types.Redirect
  alias Ellie.Types.ProjectId
  alias Data.Functor

  @behaviour Ellie.Domain.Api

  @spec create_revision(Ellie.Domain.Api.new_revision) :: {:ok, Revision.t} | :error
  def create_revision(data) do
    result =
      data
      |> Keyword.put(:elm_version, Platform.latest_version)
      |> Keyword.to_list()
      |> (&Kernel.struct(Revision, &1)).()
      |> Repo.insert()
    case result do
      {:ok, revision} -> {:ok, revision}
      _ -> :error
    end
  end

  @spec retrieve_revision(ProjectId.t, integer) :: Revision.t | nil
  def retrieve_revision(project_id, revision_number) do
    redirect =
      Redirect
      |> Repo.get_by(project_id: project_id, revision_number: revision_number)
      |> Repo.preload(:revision)
      |> Functor.map(&(&1.revision))
    case redirect do
      nil -> mirror_from_s3(project_id, revision_number)
      revision -> revision
    end
  end

  @spec retrieve_revision(ProjectId.t) :: Revision.t | nil
  def retrieve_revision(id) do
    Repo.get(Revision, id)
  end

  defp parse_package([name_string, version_string]) do
    with {:ok, name} <- Name.from_string(name_string),
         {:ok, version} <- Version.from_string(version_string)
    do
      {:ok, %Package{name: name, version: version}}
    else
      _ -> :error
    end
  end
  defp parse_package(_), do: :error

  defp mirror_from_s3(project_id, revision_number) do
    endpoint = Keyword.get(
      Application.get_env(:ellie, Ellie.Domain.Api, []),
      :legacy_revisions_endpoint,
      "https://s3.us-east-2.amazonaws.com/development-cdn.ellie-app.com/revisions"
    )

    url = "#{endpoint}/#{ProjectId.to_string(project_id)}/#{revision_number}.json"

    with {:ok, %HTTPoison.Response{status_code: 200, body: body}} <- HTTPoison.get(url),
         {:ok, revision_data} <- Poison.decode(body),
         {:ok, package_combos} <- Map.fetch(revision_data, "packages"),
         {:ok, packages} <- EnumHelpers.traverse_result(package_combos, &parse_package/1),
         {:ok, html_code} <- Map.fetch(revision_data, "htmlCode"),
         {:ok, elm_code} <- Map.fetch(revision_data, "elmCode"),
         {:ok, %{"projectId"=>project_id_string, "revisionNumber"=>revision_number}} <- Map.fetch(revision_data, "id"),
         title <- Map.get(revision_data, "title"),
         terms_version <- Map.get(revision_data, "acceptedTerms")
    do
      result =
        Repo.transaction fn ->
          revision = %Revision{
            html_code: html_code,
            elm_code: elm_code,
            title: title,
            elm_version: Version.create(0, 18, 0),
            packages: packages,
            terms_version: terms_version
          }

          inserted = Repo.insert!(revision)

          redirect = %Redirect{
            project_id: ProjectId.from_string(project_id_string),
            revision_number: revision_number,
            revision: inserted
          }

          Repo.insert!(redirect)

          inserted
        end
      case result do
        {:ok, saved_revision} ->
          saved_revision
        _error ->
          # TODO LOG ERROR
          nil
      end
    else
      error ->
        # TODO LOG ERROR
        IO.inspect(error)
        nil
    end
  end
end

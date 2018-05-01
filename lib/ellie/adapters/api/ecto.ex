defmodule Ellie.Adapters.Api.Ecto do
  import Ecto.Query
  alias Ellie.Repo
  alias Elm.Package
  alias Elm.Version
  alias Ellie.Types.Revision
  alias Ellie.Types.ProjectId
  alias Ellie.Types.Settings
  alias Ellie.Types.User

  @behaviour Ellie.Domain.Api

  def accept_terms(user, terms_version) do
    changeset = Ecto.Changeset.change(user, terms_version: terms_version)
    case Repo.update(changeset) do
      {:ok, _} -> :ok
      {:error, _} -> :error
    end
  end

  @spec update_settings(User.t, Keyword.t) :: :ok | :error
  def update_settings(user, settings) do
    changeset = Ecto.Changeset.change(user, settings: struct(user.settings, settings))
    case Repo.update(changeset) do
      {:ok, _} -> :ok
      {:error, _} -> :error
    end
  end

  @type new_revision :: [title: String.t | nil, elm_code: String.t, html_code: String.t, packages: list(Package.t)]
  @spec create_revision(User.t, new_revision) :: {:ok, Revision.t} | :error
  def create_revision(user, data) do
    case user.terms_version do
      nil ->
        :error
      terms_version ->
        result =
          data
          |> Keyword.put(:elm_version, Version.create(0, 19, 0))
          |> Keyword.put(:terms_version, terms_version)
          |> Keyword.put(:user, user)
          |> Keyword.to_list()
          |> (&Kernel.struct(Revision, &1)).()
          |> Repo.insert()
        case result do
          {:ok, revision} -> {:ok, revision}
          _ -> :error
        end
    end
  end

  @type updated_revision :: [
    project_id: ProjectId.t,
    title: String.t | nil,
    elm_code: String.t,
    html_code: String.t,
    packages: list(Package.t)
  ]
  @spec update_revision(User.t, updated_revision) :: {:ok, Revision.t} | :error
  def update_revision(user, data) do
    case user.terms_version do
      nil ->
        :error
      terms_version ->
        project_id =
          Keyword.fetch!(data, :project_id)
        latest_revision_number =
          Revision
          |> where([r], r.project_id == ^project_id)
          |> select([r], max(r.revision_number))
          |> Repo.one()
        if is_nil(latest_revision_number) do
          :error
        else
          result =
            data
            |> Keyword.put(:revision_number, latest_revision_number + 1)
            |> Keyword.put(:elm_version, Version.create(0, 19, 0))
            |> Keyword.put(:terms_version, terms_version)
            |> Keyword.put(:user, user)
            |> Keyword.to_list()
            |> (&Kernel.struct(Revision, &1)).()
            |> Repo.insert()
          case result do
            {:ok, revision} -> {:ok, revision}
            _ -> :error
          end
        end
    end
  end

  @spec retrieve_revision(ProjectId.t, integer) :: Revision.t | nil
  def retrieve_revision(project_id, revision_number) do
    Repo.get_by(Revision, project_id: project_id, revision_number: revision_number)
  end

  @spec create_user() :: {:ok, User.t} | :error
  def create_user() do
    case Repo.insert(%User{settings: Settings.default()}) do
      {:ok, user} -> {:ok, user}
      _ -> :error
    end
  end
end

defmodule Ellie.Domain.Api do
  alias Ellie.Types.ProjectId
  alias Ellie.Types.Revision
  alias Ellie.Types.User
  alias Ellie.Types.Revision

  @type revision_number :: integer

  @type new_revision :: [
    title: String.t | nil,
    elm_code: String.t,
    html_code: String.t,
    packages: list(Package.t)
  ]

  @type updated_revision :: [
    project_id: ProjectId.t,
    title: String.t | nil,
    elm_code: String.t,
    html_code: String.t,
    packages: list(Package.t)
  ]

  @callback accept_terms(User.t, integer) :: :ok | :error
  @callback update_settings(User.t, Keyword.t) :: :ok | :error
  @callback create_revision(User.t, new_revision) :: {:ok, Revision.t} | :error
  @callback update_revision(User.t, updated_revision) :: {:ok, Revision.t} | :error
  @callback retrieve_revision(ProjectId.t, revision_number) :: Revision.t | nil
  @callback create_user() :: {:ok, User.t} | :error

  @spec accept_terms(user :: User.t, terms :: integer) :: :ok | :error
  def accept_terms(user, terms) do
    adapter().accept_terms(user, terms)
  end

  @spec update_settings(user :: User.t, settings :: Keyword.t) :: :ok | :error
  def update_settings(user, settings) do
    adapter().update_settings(user, settings)
  end

  @spec create_revision(user :: User.t, revision :: new_revision) :: {:ok, Revision.t} | :error
  def create_revision(user, revision) do
    adapter().create_revision(user, revision)
  end

  @spec update_revision(user :: User.t, revision :: updated_revision) :: {:ok, Revision.t} | :error
  def update_revision(user, revision) do
    adapter().update_revision(user, revision)
  end

  @spec retrieve_revision(project_id :: ProjectId.t, revision_number :: integer) :: Revision.t | nil
  def retrieve_revision(project_id, revision_number) do
    adapter().retrieve_revision(project_id, revision_number)
  end

  @spec create_user() :: {:ok, User.t} | :error
  def create_user() do
    adapter().create_user()
  end

  defp adapter() do
    Application.get_env(:ellie, Ellie.Domain.Api, [])
    |> Keyword.get(:adapter, Ellie.Adapters.Api.Ecto)
  end
end

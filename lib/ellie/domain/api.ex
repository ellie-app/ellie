defmodule Ellie.Domain.Api do
  alias Ellie.Types.PrettyId
  alias Ellie.Types.Revision
  alias Ellie.Types.Revision

  @type revision_number :: integer

  @type new_revision :: [
          title: String.t() | nil,
          elm_code: String.t(),
          html_code: String.t(),
          packages: list(Package.t()),
          terms_version: integer
        ]

  @callback create_revision(new_revision) :: {:ok, Revision.t()} | :error
  @callback retrieve_revision(PrettyId.t(), revision_number) :: Revision.t() | nil
  @callback retrieve_revision(PrettyId.t()) :: Revision.t() | nil

  @spec create_revision(revision :: new_revision) :: {:ok, Revision.t()} | :error
  def create_revision(revision) do
    adapter().create_revision(revision)
  end

  @spec retrieve_revision(project_id :: PrettyId.t(), revision_number :: integer) ::
          Revision.t() | nil
  def retrieve_revision(project_id, revision_number) do
    adapter().retrieve_revision(project_id, revision_number)
  end

  @spec retrieve_revision(id :: PrettyId.t()) :: Revision.t() | nil
  def retrieve_revision(id) do
    adapter().retrieve_revision(id)
  end

  defp adapter() do
    Application.get_env(:ellie, Ellie.Domain.Api, [])
    |> Keyword.get(:adapter, Ellie.Adapters.Api.Ecto)
  end
end

defmodule EllieWeb.PageController do
  use EllieWeb, :controller
  alias Ellie.Domain.Api
  alias Ellie.Types.PrettyId

  def assets_fallback(conn, _params), do: send_resp(conn, :not_found, "")

  def new_editor(conn, _params), do: render(conn, "editor.html")

  def existing_editor_old(conn, params),
    do: old_id_scheme_redirect(:existing_editor, :new_editor, conn, params)

  def existing_editor(conn, _params), do: render(conn, "editor.html")

  def embed_old(conn, params), do: old_id_scheme_redirect(:embed, :embed, conn, params)
  def embed(conn, _params), do: render(conn, "embed.html")

  def terms(conn, params), do: render(conn, "terms/v#{Map.get(params, "version")}/index.html")

  defp old_id_scheme_redirect(redirection, not_found, conn, %{
         "project_id" => project_id_param,
         "revision_number" => revision_number_param
       }) do
    with {:ok, project_id} <- PrettyId.cast(project_id_param),
         {revision_number, _} <- Integer.parse(revision_number_param),
         revision when not is_nil(revision) <- Api.retrieve_revision(project_id, revision_number) do
      conn
      |> put_status(:moved_permanently)
      |> redirect(to: page_path(conn, redirection, to_string(revision.id)))
      |> halt()
    else
      _ ->
        redirect(conn, to: page_path(conn, not_found, "not-found"))
    end
  end

  defp old_id_scheme_redirect(_redirection, _not_found, conn, _params), do: conn
end

defmodule EllieWeb.OembedController do
  use EllieWeb, :controller
  alias Ellie.Types.PrettyId
  alias Ellie.Domain.Api

  def oembed(conn, %{"url" => url} = params) do
    parsed = URI.parse(url)
    current = URI.parse(current_url(conn))
    if parsed.authority != current.authority do
      conn
      |> send_resp(404, "")
      |> halt()
    else
      case String.split(String.trim(parsed.path, "/"), "/") do
        [project_id_string, revision_number_string] ->
          for_legacy_url(conn, project_id_string, revision_number_string, params)
        [id_string] ->
          for_current_url(conn, id_string, params)
        _ ->
          conn
          |> send_resp(404, "")
          |> halt()
      end
    end
  end

  defp for_legacy_url(conn, project_id_string, revision_number_string, params) do
    with {revision_number, _str} <- Integer.parse(revision_number_string),
         {:ok, project_id} <- PrettyId.cast(project_id_string),
         revision when not is_nil(revision) <- Api.retrieve_revision(project_id, revision_number)
    do
      render conn, "index.json",
        revision: revision,
        width: Map.get(params, "width", 800),
        height: Map.get(params, "height", 400)
    else
      _ ->
        conn
        |> send_resp(404, "")
        |> halt()
    end
  end

  defp for_current_url(conn, id_string, params) do
    with {:ok, id} <- PrettyId.cast(id_string),
         revision when not is_nil(revision) <- Api.retrieve_revision(id)
    do
      render conn, "index.json",
        revision: revision,
        width: Map.get(params, "width", 800),
        height: Map.get(params, "height", 400)
    else
      _ -> send_resp(conn, 404, "")
    end
  end
end

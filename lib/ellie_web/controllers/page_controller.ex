defmodule EllieWeb.PageController do
  use EllieWeb, :controller
  alias Ellie.Domain.Embed
  alias Ellie.Domain.Workspace
  alias Ellie.Repo
  alias Ellie.Types.Revision
  alias Ellie.Types.ProjectId

  def new_editor(conn, _params) do
    conn
    |> render("editor.html")
  end

  def existing_editor(conn, _params) do
    conn
    |> render("editor.html")
  end

  def embed(conn, _params) do
    conn
    |> render("embed.html")
  end

  def terms(conn, params) do
    conn
    |> render("terms_v#{Map.get(params, "version")}.html")
  end

  def result_manager(conn, params) do
    with token <- Map.get(params, "token"),
         {:ok, user} <- EllieWeb.Auth.verify(token),
         {:ok, {path, hash}} <- Workspace.result(user)
    do
      hash_string = Integer.to_string(hash, 16)
      if hash_string in get_req_header(conn, "if-none-match") do
        conn
        |> send_resp(304, "")
        |> halt()
      else
        conn
        |> put_resp_header("ETag", hash_string)
        |> put_resp_content_type("application/javascript")
        |> send_file(200, path)
      end
    else
      _ -> send_chunked(conn, 404)
    end
  end

  def embed_result(conn, %{"project_id" => project_id, "revision_number" => revision_number}) do
    with parsed_project_id <- ProjectId.from_string(project_id),
         revision when not is_nil(revision) <- Repo.get_by(Revision, project_id: parsed_project_id, revision_number: revision_number),
         {:ok, path} <- Embed.result(revision)
    do
      conn
      |> put_resp_content_type("application/javascript")
      |> send_file(200, path)
    else
      _ ->
        send_chunked(conn, 404)
    end
  end

  def redirect_sw(conn, _params) do
    case HTTPoison.get("http://localhost:8080/ServiceWorker.js") do
      { :ok, response } -> conn |> put_resp_content_type("application/javascript") |> send_resp(response.status_code, response.body)
      { :error, _error } -> conn |> put_status(:bad_gateway) |> halt()
    end
  end
end

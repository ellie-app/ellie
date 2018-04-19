defmodule EllieWeb.PageController do
  use EllieWeb, :controller
  alias Ellie.Elm.Version
  alias Ecto.UUID
  alias Ellie.Embed

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

  def result(conn, params) do
    with token <- Map.get(params, "token"),
      {:ok, user} <- EllieWeb.Auth.verify(token),
      {:ok, version} <- Version.from_string(Map.get(params, "elmVersion", "")),
      {:ok, path} <- Ellie.Workspace.result(user, version)
    do
      conn
      |> put_resp_content_type("application/javascript")
      |> send_file(200, path)
    else
      _ -> conn
    end
  end

  def embed_result(conn, %{"project_id" => project_id, "revision_number" => revision_number}) do
    with {:ok, uuid} <- UUID.cast(project_id),
      {int, _str} <- Integer.parse(revision_number),
      {:ok, path} <- Embed.result(uuid, int)
    do
      conn
      |> put_resp_content_type("application/javascript")
      |> send_file(200, path)
    else
      error ->
        IO.inspect(error)
        send_chunked(conn, 404)
    end
  end
end

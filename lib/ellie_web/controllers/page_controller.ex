defmodule EllieWeb.PageController do
  use EllieWeb, :controller
  alias Ellie.Elm.Version

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
end

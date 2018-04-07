defmodule EllieWeb.PageController do
  use EllieWeb, :controller

  def new_editor(conn, _params) do
    conn
      |> render("editor.html")
  end

  def existing_editor(conn, _params) do
    conn
      |> render("editor.html")
  end
end

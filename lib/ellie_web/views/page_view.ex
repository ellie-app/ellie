defmodule EllieWeb.PageView do
  use EllieWeb, :view

  def production?, do: Application.get_env(:ellie, :env) == :prod
  def development?, do: Application.get_env(:ellie, :env) == :dev

  def asset(conn, :editor_js), do: handle_relative(conn, "editor.js")
  def asset(conn, :editor_css), do: handle_relative(conn, "editor.css")
  def asset(conn, :embed_js), do: handle_relative(conn, "embed.js")
  def asset(conn, :embed_css), do: handle_relative(conn, "embed.css")
  def asset(_, _), do: ""

  defp handle_relative(conn, relative) do
    if production?() do
      static_path(conn, "/" <> relative)
    else
      "http://localhost:8080/" <> relative
    end
  end
end

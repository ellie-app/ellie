defmodule EllieWeb.PageView do
  use EllieWeb, :view

  def production?, do: Application.get_env(:ellie, :env) == :prod
  def development?, do: Application.get_env(:ellie, :env) == :dev

  def asset(:editor_js), do: webpack_path("editor.js")
  def asset(:editor_css), do: webpack_path("editor.css")
  def asset(:embed_js), do: webpack_path("embed.js")
  def asset(:embed_css), do: webpack_path("embed.css")
  def asset(_), do: ""

  defp webpack_path(relative), do: "http://localhost:8080/" <> relative
end

defmodule EllieWeb.PageView do
  use EllieWeb, :view
  def production?, do: Application.get_env(:ellie, :env) == :prod
  def development?, do: Application.get_env(:ellie, :env) == :dev
  def oembed_url(conn) do
    current = URI.parse(Phoenix.Controller.current_url(conn))
    base = current.scheme <> "://" <> current.authority
    base <> "/oembed?url=" <> Phoenix.Controller.current_url(conn)
  end
end

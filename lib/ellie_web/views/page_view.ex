defmodule EllieWeb.PageView do
  use EllieWeb, :view
  def production?, do: Application.get_env(:ellie, :env) == :prod
  def development?, do: Application.get_env(:ellie, :env) == :dev
end

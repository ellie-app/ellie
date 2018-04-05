defmodule EllieWeb.PageController do
  use EllieWeb, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end

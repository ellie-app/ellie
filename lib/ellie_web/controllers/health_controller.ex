defmodule EllieWeb.HealthController do
  use EllieWeb, :controller

  def healthz(conn, _) do
    conn
    |> put_status(:ok)
    |> json(%{nothing: true})
  end
end

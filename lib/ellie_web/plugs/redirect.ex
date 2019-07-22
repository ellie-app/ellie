defmodule EllieWeb.Plugs.Redirect do
  import Plug.Conn

  @behaviour Plug

  def init(opts), do: opts

  def call(conn, _) do
    case conn.host do
      "www." <> bare_domain ->
        conn
        |> put_status(:moved_permanently)
        |> Phoenix.Controller.redirect(
          external: "#{conn.scheme}://#{bare_domain}#{conn.request_path}"
        )

      _ ->
        conn
    end
  end
end

defmodule EllieWeb.Context do
  @behaviour Plug

  import Plug.Conn
  alias Ecto.Query
  alias EllieWeb.Auth

  def init(opts), do: opts

  def call(conn, _) do
    case build_context(conn) do
      {:ok, context} ->
        put_private(conn, :absinthe, %{context: context})
      {:error, reason} ->
        conn
        |> send_resp(403, reason)
        |> halt()
      _ ->
        conn
        |> send_resp(400, "Bad Request")
        |> halt()
    end
  end

  def build_context(conn) do
    with ["Bearer " <> auth_token] <- get_req_header(conn, "authorization"),
         {:ok, current_user}       <- Auth.verify(auth_token)
    do
      {:ok, %{current_user: current_user}}
    else
      []    -> {:ok, %{}}
      error -> error
    end
  end

end

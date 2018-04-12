defmodule EllieWeb.Context do
  @behaviour Plug

  import Plug.Conn
  alias EllieWeb.Auth

  def init(opts), do: opts

  def call(conn, _) do
    put_private(conn, :absinthe, build_context(conn))
      # {:error, _reason} ->
      #   conn
      #     |> send_resp(403, "Unauthorized")
      #     |> halt()
      # _ ->
      #   conn
      #     |> send_resp(400, "Bad Request")
      #     |> halt()
    # end
  end

  def build_context(conn) do
    with ["Bearer " <> auth_token] <- get_req_header(conn, "authorization"),
      {:ok, current_user} <- Auth.verify(auth_token)
    do
      IO.puts("user found")
      %{context: %{current_user: current_user}}
    else
      error ->
        IO.puts("user not found")
        IO.inspect(error)
        %{}
    end
  end
end

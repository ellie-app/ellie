defmodule EllieWeb.Graphql.Context do
  import Plug.Conn
  alias EllieWeb.Token
  alias Data.Uuid

  @behaviour Plug

  def init(opts), do: opts

  def call(conn, _) do
    with ["Bearer " <> auth_token] <- get_req_header(conn, "authorization"),
         {:ok, workspace_string} <- Token.verify(auth_token),
         {:ok, workspace} <- Uuid.parse(workspace_string)
    do
      put_private(conn, :absinthe, %{context: %{workspace: workspace}})
    else
      _ -> conn
    end
  end
end

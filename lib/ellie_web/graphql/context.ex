defmodule EllieWeb.Graphql.Context do
  import Plug.Conn
  alias EllieWeb.Token
  alias Data.Uuid

  @behaviour Plug

  def init(opts), do: opts

  def call(conn, _) do
    with ["Bearer " <> auth_token] <- get_req_header(conn, "authorization"),
         {:ok, workspace_string} <- Token.verify(auth_token),
         {:ok, workspace} <- Uuid.parse(workspace_string) do
      put_private(conn, :absinthe, %{
        context: %{
          workspace: workspace,
          original: Map.get(conn.params, "query")
        }
      })
    else
      _ ->
        put_private(conn, :absinthe, %{
          context: %{
            original: Map.get(conn.params, "query")
          }
        })
    end
  end
end

defmodule EllieWeb.Graphql.Socket do
  use Phoenix.Socket
  use Absinthe.Phoenix.Socket, schema: EllieWeb.Graphql.Schema
  alias Ellie.Domain.Workspace
  alias EllieWeb.Token
  alias Data.Uuid

  transport(:websocket, Phoenix.Transports.WebSocket, timeout: 45_000)

  def connect(%{"token" => token}, socket) do
    with {:ok, workspace_string} <- Token.verify(token),
         {:ok, workspace} <- Uuid.parse(workspace_string),
         :unit <- Workspace.watch(workspace, socket.transport_pid) do
      {:ok, Absinthe.Phoenix.Socket.put_options(socket, context: %{workspace: workspace})}
    else
      _ -> {:error, "Socket setup failed"}
    end
  end

  def connect(_params, socket) do
    {:ok, socket}
  end

  def id(_socket), do: nil
end

defmodule EllieWeb.UserSocket do
  use Phoenix.Socket
  use Absinthe.Phoenix.Socket, schema: EllieWeb.Graphql.Schema
  alias EllieWeb.Auth
  alias Ellie.Domain.Workspace

  transport :websocket, Phoenix.Transports.WebSocket,
    timeout: 45_000

  def connect(%{ "token" => token }, socket) do
    case Auth.verify(token) do
      {:ok, user} ->
        Workspace.cleanup_after(user, socket.transport_pid)
        {:ok, Absinthe.Phoenix.Socket.put_options(socket, context: %{current_user: user})}
      _ ->
        {:error, "Not authorized"}
    end
  end

  def connect(_params, socket) do
    {:ok, socket}
  end

  def id(_socket), do: nil
end

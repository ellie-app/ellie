defmodule EllieWeb.UserSocket do
  use Phoenix.Socket
  use Absinthe.Phoenix.Socket, schema: EllieWeb.Schema
  alias EllieWeb.Auth

  transport :websocket, Phoenix.Transports.WebSocket

  def connect(params, socket) do
    token = params["token"]
    {:ok, user} = Auth.verify(token)
    send(self(), :hello)
    socket = Absinthe.Phoenix.Socket.put_options(socket, context: %{current_user: user})
    {:ok, socket}
  end

  def id(_socket), do: nil
end

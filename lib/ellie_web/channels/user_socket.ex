defmodule EllieWeb.UserSocket do
  use Phoenix.Socket
  use Absinthe.Phoenix.Socket, schema: EllieWeb.Graphql.Schema
  alias EllieWeb.Auth

  transport :websocket, Phoenix.Transports.WebSocket

  def connect(%{ "token" => token }, socket) do
    case Auth.verify(token) do
      {:ok, user} ->
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

defmodule EllieWeb.Serializers.Msgpack do
  @behaviour Phoenix.Transports.Serializer
  alias Phoenix.Socket.Reply
  alias Phoenix.Socket.Message
  alias Phoenix.Socket.Broadcast

  def fastlane!(%Broadcast{} = msg) do
    msg = %Message{topic: msg.topic, event: msg.event, payload: msg.payload}
    {:socket_push, :binary, encode_to_binary(msg)}
  end

  def encode!(%Reply{} = reply) do
    msg = %Message{
      topic: reply.topic,
      event: "phx_reply",
      ref: reply.ref,
      payload: %{status: reply.status, response: reply.payload},
      join_ref: reply.join_ref
    }
    {:socket_push, :binary, encode_to_binary(msg)}
  end
  def encode!(%Message{} = msg) do
    {:socket_push, :binary, encode_to_binary(msg)}
  end

  defp encode_to_binary(msg) do
    msg
    |> Map.from_struct()
    |> Msgpax.pack!()
  end

  def decode!(message, _opts) do
    message
    |> Msgpax.unpack!()
    |> Phoenix.Socket.Message.from_map!()
  end
end

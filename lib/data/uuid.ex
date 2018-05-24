defmodule Data.Uuid do
  @type t :: binary

  @spec new() :: t
  def new() do
    Ecto.UUID.bingenerate()
  end

  @spec to_string(t) :: String.t
  def to_string(uuid) do
    case Ecto.UUID.load(uuid) do
      {:ok, string} -> string
      _error -> raise ArgumentError, "Malformed UUID. This should never happen."
    end
  end

  @spec parse(string :: String.t) :: {:ok, t} | :error
  def parse(string) when is_binary(string) do
    case Ecto.UUID.dump(string) do
      {:ok, uuid} -> {:ok, uuid}
      _error -> :error
    end
  end
  def parse(_input), do: :error
end

defmodule EllieWeb.Token do
  @salt "EllieWeb.Token"

  @spec sign(string :: String.t) :: String.t
  def sign(string) when is_binary(string) do
    Phoenix.Token.sign(EllieWeb.Endpoint, @salt, string)
  end

  @spec verify(string :: String.t) :: {:ok, String.t} | :error
  def verify(string) when is_binary(string) do
    case Phoenix.Token.verify(EllieWeb.Endpoint, @salt, string, max_age: :infinity) do
      {:ok, string} -> {:ok, string}
      _error -> :error
    end
  end
end

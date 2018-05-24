defmodule Data.Json do
  alias Data.Result

  @spec decode(json :: String.t()) :: Result.t(String.t() | nil, any())
  def decode(json) do
    case Poison.decode(json, []) do
      {:error, :invalid, _} -> {:error, nil}
      {:error, {:invalid, reason, _}} -> {:error, reason}
      {:ok, stuff} -> {:ok, stuff}
    end
  rescue
    _ -> {:error, nil}
  end


  @spec encode(any()) :: Result.t(String.t() | nil, String.t())
  def encode(data) do
    case Poison.encode(data) do
      {:error, {:invalid, _}} -> {:error, nil}
      {:ok, string} -> {:ok, string}
    end
  rescue
    _ -> {:error, nil}
  end
end

defmodule Ellie.Types.PrettyId do
  defstruct [:value]

  @opaque t :: %__MODULE__{value: integer}

  @behaviour Ecto.Type

  def type, do: :bigint

  @spec cast(value :: any) :: {:ok, t} | :error
  def cast(%__MODULE__{} = pretty_id), do: {:ok, pretty_id}
  def cast(value) when is_integer(value), do: {:ok, %__MODULE__{value: value}}
  def cast(value) when is_binary(value), do: from_string(value)
  def cast(_), do: :error

  @spec load(value :: any) :: {:ok, t} | :error
  def load(value) when is_integer(value), do: {:ok, %__MODULE__{value: value}}
  def load(_), do: :error

  @spec dump(value :: any) :: {:ok, integer} | :error
  def dump(%__MODULE__{value: value}), do: {:ok, value}
  def dump(_), do: :error

  @spec to_string(pretty_id :: t) :: String.t()
  def to_string(pretty_id), do: serialize_help(pretty_id.value, "")

  defimpl String.Chars do
    def to_string(value), do: Ellie.Types.PrettyId.to_string(value)
  end

  @alphabet "23456789bcdfghjkmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ"
            |> String.graphemes()
            |> Enum.with_index()
            |> Enum.map(fn {v, k} -> {k, v} end)
            |> Map.new()

  @inverse_alphabet @alphabet
                    |> Enum.map(fn {k, v} -> {v, k} end)
                    |> Map.new()

  @base_length Enum.count(@alphabet)

  defp serialize_help(tracker, output) do
    if tracker <= 0 do
      output <> "a1"
    else
      serialize_help(
        div(tracker, @base_length),
        Map.fetch!(@alphabet, rem(tracker, @base_length)) <> output
      )
    end
  end

  defp from_string_v1(input) do
    input
    |> String.replace("a1", "")
    |> String.graphemes()
    |> Enum.reduce_while({:ok, 0}, fn char, {:ok, tracker} ->
      case Map.fetch(@inverse_alphabet, char) do
        {:ok, index} -> {:cont, {:ok, tracker * @base_length + index}}
        _ -> {:halt, :error}
      end
    end)
    |> from_result()
  end

  defp from_string_v0(input) do
    input
    |> String.graphemes()
    |> Enum.reduce_while({:ok, 0}, fn char, {:ok, tracker} ->
      case Map.fetch(@inverse_alphabet, char) do
        {:ok, index} -> {:cont, {:ok, tracker * @base_length + index + 1}}
        _ -> {:halt, :error}
      end
    end)
    |> from_result()
  end

  defp from_string(value) when is_binary(value) do
    if String.ends_with?(value, "a1") do
      from_string_v1(value)
    else
      from_string_v0(value)
    end
  end

  defp from_result({:ok, integer}), do: {:ok, %__MODULE__{value: integer}}
  defp from_result(:error), do: :error
end

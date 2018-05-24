defmodule Ellie.Types.ProjectId do
  @alphabet "23456789bcdfghjkmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ"
            |> String.graphemes()
            |> Enum.with_index()
            |> Enum.map(fn {v, k} -> {k, v} end)
            |> Map.new()

  @inverse_alphabet @alphabet
                    |> Enum.map(fn {k, v} -> {v, k} end)
                    |> Map.new()

  @base_length Enum.count(@alphabet)

  @type t :: integer

  @spec to_string(value :: t) :: String.t
  def to_string(input) when is_integer(input) do
    serialize_help(input, "")
  end

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
    |> Enum.reduce(0, fn char, tracker ->
      tracker * @base_length + Map.fetch!(@inverse_alphabet, char)
    end)
  end

  defp from_string_v0(input) do
    input
    |> String.graphemes()
    |> Enum.reduce(0, fn char, tracker ->
      tracker * @base_length + (Map.fetch!(@inverse_alphabet, char) + 1)
    end)
  end

  @spec from_string(value :: String.t) :: t
  def from_string(value) when is_binary(value) do
    if String.ends_with?(value, "a1") do
      from_string_v1(value)
    else
      from_string_v0(value)
    end
  end
end

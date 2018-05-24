defmodule Ellie.Helpers.EnumHelpers do
  def traverse_result(list, func) do
    case Enum.reduce_while(list, {:ok, []}, fn a, b -> traverse_result_help(func.(a), b) end) do
      {:ok, list} -> {:ok, Enum.reverse(list)}
      error -> error
    end
  end

  defp traverse_result_help({:ok, a}, {:ok, list}), do: {:cont, {:ok, [a | list]}}
  defp traverse_result_help(_, {:error, _} = output), do: {:halt, output}
  defp traverse_result_help({:error, _} = output, _), do: {:halt, output}
  defp traverse_result_help(:error, _), do: {:halt, {:error, "list traversal failed"}}
end

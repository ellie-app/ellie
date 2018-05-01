defmodule Data.Maybe do
  @type t(a) :: {:just, a} | :nothing

  @spec map(a :: t(a), f :: (a -> b)) :: t(b) when a: var, b: var
  def map(:nothing), do: :nothing
  def map({:just, a}, f), do: {:just, f.(a)}


  @spec map2(a :: t(a), b :: t(b), f :: (a, b -> c)) :: t(c) when a: var, b: var, c: var
  def map2(a, b, f) do
    with {:just, aval} <- a,
         {:just, bval} <- b
    do
      {:just, f.(aval, bval)}
    else
      _ -> :nothing
    end
  end

  @spec and_map(fa :: t((a -> b)), a :: t(a)) :: t(b) when a: var, b: var
  def and_map(:nothing, _), do: :nothing
  def and_map(_, :nothing), do: :nothing
  def and_map({:just, f}, {:just, a}), do: {:just, f.(a)}

  @spec and_then(a :: t(a), f :: (a -> t(b))) :: t(b) when a: var, b: var
  def and_then(:nothing, _), do: :nothing
  def and_then({:just, a}, f), do: f.(a)

  @spec succeed(a :: a) :: t(a) when a: var
  def succeed(a), do: {:just, a}

  @spec traverse(list :: list(t(a)), f :: (a -> b)) :: t(list(b)) when a: var, b: var
  def traverse(list, func) do
    case Enum.reduce_while(list, succeed([]), fn a, b -> traverse_help(func.(a), b) end) do
      {:just, list} -> {:just, Enum.reverse(list)}
      :nothing -> :nothing
    end
  end

  defp traverse_help({:just, a}, {:just, list}), do: {:cont, {:just, [a | list]}}
  defp traverse_help(_, :nothing), do: {:halt, :nothing}
  defp traverse_help(:nothing, _), do: {:halt, :nothing}
end

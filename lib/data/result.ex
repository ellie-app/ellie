defmodule Data.Result do
  @type t(e, a) :: {:ok, a} | {:error, e}

  @spec map(a :: t(e, a), f :: (a -> b)) :: t(e, b) when a: var, b: var, e: var
  def map({:error, _} = e, _), do: e
  def map({:ok, a}, f), do: {:ok, f.(a)}

  @spec map_error(a :: t(e1, a), f :: (e1 -> e2)) :: t(e2, a) when a: var, e1: var, e2: var
  def map_error({:error, e}, f), do: {:error, f.(e)}
  def map_error(a, _), do: a

  @spec map2(a :: t(e, a), b :: t(e, b), f :: (a, b -> c)) :: t(e, c) when a: var, b: var, c: var, e: var
  def map2(a, b, f) do
    with {:ok, aval} <- a,
         {:ok, bval} <- b
    do
      {:ok, f.(aval, bval)}
    else
      e -> e
    end
  end

  @spec and_map(fa :: t(e, (a -> b)), a :: t(e, a)) :: t(e, b) when a: var, b: var, e: var
  def and_map({:error, _} = fne, _), do: fne
  def and_map(_, {:error, _} = e), do: e
  def and_map({:ok, f}, {:ok, a}), do: {:ok, f.(a)}

  @spec and_then(a :: t(e, a), f :: (a -> t(e, b))) :: t(e, b) when a: var, b: var, e: var
  def and_then({:error, _} = e, _), do: e
  def and_then({:ok, a}, f), do: f.(a)

  @spec succeed(a :: a) :: t(any(), a) when a: var
  def succeed(a), do: {:ok, a}

  @spec fail(e :: e) :: t(e, any()) when e: var
  def fail(e), do: {:error, e}

  @spec ensure(:ok | {:error, e}) :: Result.t(e, :unit) when e: var
  def ensure(:error), do: {:error, :unit}
  @spec ensure({:ok, a} | :error) :: Result.t(:unit, a) when a: var
  def ensure(:ok), do: {:ok, :unit}
  def ensure(a), do: a

  @spec traverse(list :: list(t(e, a)), f :: (a -> b)) :: t(e, list(b)) when a: var, b: var, e: var
  def traverse(list, func) do
    case Enum.reduce_while(list, {:ok, []}, fn a, b -> traverse_help(func.(a), b) end) do
      {:ok, list} -> {:ok, Enum.reverse(list)}
      error -> error
    end
  end

  defp traverse_help({:ok, a}, {:ok, list}), do: {:cont, {:ok, [a | list]}}
  defp traverse_help(_, {:error, _} = output), do: {:halt, output}
  defp traverse_help({:error, _} = output, _), do: {:halt, output}
end

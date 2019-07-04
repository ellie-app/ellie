defmodule Data.Function do
  def curry(fun) do
    {_, arity} = :erlang.fun_info(fun, :arity)
    curry_help(fun, arity, [])
  end

  defp curry_help(fun, 0, arguments) do
    apply(fun, Enum.reverse(arguments))
  end

  defp curry_help(fun, arity, arguments) do
    fn arg -> curry_help(fun, arity - 1, [arg | arguments]) end
  end
end

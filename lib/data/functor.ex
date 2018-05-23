defprotocol Data.Functor do
  @fallback_to_any true

  @spec map(t, (term -> term)) :: t
  def map(value, fun)
end


defimpl Data.Functor, for: Any do
  def map(nil, fun) when is_function(fun, 1), do: nil

  def map(value, fun) when is_function(fun, 1) do
    fun.(value)
  end
end

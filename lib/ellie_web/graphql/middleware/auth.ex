defmodule EllieWeb.Graphql.Middleware.Auth do
  @behaviour Absinthe.Middleware

  def call(resolution = %{context: %{current_user: _}}, _config) do
    resolution
  end

  def call(resolution, _config) do
    resolution
    |> Absinthe.Resolution.put_result({:error, %{code: :not_authenticated,
                                                 error: "Not authenticated",
                                                 message: "Not authenticated"}})
    |> Kernel.struct!(state: :resolved)
  end
end

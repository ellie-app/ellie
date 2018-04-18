defmodule EllieWeb.Graphql.Resolvers.Compile do
  alias Ellie.Workspace

  def call(%{elm_version: elm_version, elm_code: elm_code, packages: packages}, %{context: %{current_user: user}}) do
    Task.start(fn ->
      {:ok, error} = Workspace.compile(user, elm_version, elm_code, MapSet.new(packages))
      Absinthe.Subscription.publish(EllieWeb.Endpoint, %{error: error}, workspace: user.id)
    end)
    {:ok, true}
  end

  def call(_args, _context) do
    {:ok, false}
  end
end

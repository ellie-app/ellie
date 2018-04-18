defmodule EllieWeb.Graphql.Resolvers.AttachToWorkspace do
  alias Ellie.Workspace

  def call(args, %{context: %{current_user: user}}) do
    Task.start(fn ->
      {:ok, workspace} = Workspace.open(user, args.elm_version)
      packages = MapSet.to_list(workspace.packages)
      Absinthe.Subscription.publish(EllieWeb.Endpoint, %{packages: packages}, workspace: user.id)
    end)
    {:ok, true}
  end

  def call(_args, _context) do
    {:ok, false}
  end
end

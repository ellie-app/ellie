defmodule EllieWeb.Schema do
  use Absinthe.Schema
  import_types EllieWeb.Graphql.Types.Uuid
  import_types EllieWeb.Graphql.Types.Version
  import_types EllieWeb.Graphql.Types.Name
  import_types EllieWeb.Graphql.Types
  alias EllieWeb.Graphql.Resolvers.Authenticate

  query do
    @desc "Get default settings"
    field :user, non_null(:user) do
      middleware EllieWeb.Graphql.Middleware.Auth
      resolve fn _, %{context: %{current_user: current_user}} -> {:ok, current_user} end
    end

    field :revision, non_null(:revision) do
      arg :project_id, non_null(:uuid)
      arg :revision_number, non_null(:integer)
      resolve &EllieWeb.Graphql.Resolvers.Revision.call/2
    end
  end

  mutation do
    field :authenticate, non_null(:user_auth) do
      resolve &Authenticate.call/2
    end

    field :create_revision, non_null(:revision) do
      arg :inputs, non_null(:revision_update_input)
      resolve &EllieWeb.Graphql.Resolvers.CreateRevision.call/2
    end
  end
end

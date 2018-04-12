defmodule EllieWeb.Schema do
  use Absinthe.Schema
  import_types EllieWeb.Graphql.Types.Uuid
  import_types EllieWeb.Graphql.Types.Version
  import_types EllieWeb.Graphql.Types.Name
  import_types EllieWeb.Graphql.Types.Elm.Error
  import_types EllieWeb.Graphql.Types

  query do
    field :user, non_null(:user) do
      middleware EllieWeb.Graphql.Middleware.Auth
      resolve fn _, %{context: %{current_user: current_user}} -> {:ok, current_user} end
    end


    field :revision, non_null(:revision) do
      arg :project_id, non_null(:uuid)
      arg :revision_number, non_null(:integer)
      resolve &EllieWeb.Graphql.Resolvers.Revision.call/2
    end

    field :package_search, non_null(list_of(non_null(:package))) do
      arg :query, non_null(:string)
      resolve &EllieWeb.Graphql.Resolvers.SearchPackages.call/2
    end
  end

  mutation do
    field :authenticate, non_null(:user_auth) do
      resolve &EllieWeb.Graphql.Resolvers.Authenticate.call/2
    end

    field :create_revision, non_null(:revision) do
      arg :inputs, non_null(:revision_update_input)
      middleware EllieWeb.Graphql.Middleware.Auth
      resolve &EllieWeb.Graphql.Resolvers.CreateRevision.call/2
    end

    field :update_revision, non_null(:revision) do
      arg :inputs, non_null(:revision_update_input)
      arg :project_id, non_null(:uuid)
      middleware EllieWeb.Graphql.Middleware.Auth
      resolve &EllieWeb.Graphql.Resolvers.UpdateRevision.call/2
    end

    field :accept_terms, non_null(:boolean) do
      arg :terms, non_null(:integer)
      middleware EllieWeb.Graphql.Middleware.Auth
      resolve &EllieWeb.Graphql.Resolvers.AcceptTerms.call/2
    end

    field :attach_to_workspace, non_null(:boolean) do
      middleware EllieWeb.Graphql.Middleware.Auth
      resolve &EllieWeb.Graphql.Resolvers.AttachToWorkspace.call/2
    end

    field :format_code, non_null(:string) do
      arg :code, non_null(:string)
      resolve &EllieWeb.Graphql.Resolvers.FormatCode.call/2
    end

    field :compile, non_null(:boolean) do
      arg :elm_code, non_null(:string)
      arg :packages, non_null(list_of(non_null(:package_input)))
      middleware EllieWeb.Graphql.Middleware.Auth
      resolve &EllieWeb.Graphql.Resolvers.Compile.call/2
    end

    field :update_settings, non_null(:boolean) do
      arg :font_size, :string
      arg :font_family, :string
      arg :theme, :theme
      arg :vim_mode, :boolean
      middleware EllieWeb.Graphql.Middleware.Auth
      resolve &EllieWeb.Graphql.Resolvers.UpdateSettings.call/2
    end
  end

  subscription do
    field :workspace, non_null(:workspace_update) do
      config fn
        _args, %{context: %{current_user: user}} -> {:ok, topic: to_string(user.id)}
        _args, _ -> {:error, "unauthorized"}
      end
    end
  end
end

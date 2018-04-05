defmodule EllieWeb.Schema do
  use Absinthe.Schema
  import_types EllieWeb.Graphql.Types
  import_types EllieWeb.Graphql.Types.Uuid
  alias EllieWeb.Graphql.Resolvers.Authenticate

  query do
    @desc "Get default settings"
    field :default_settings, non_null(:settings) do
      middleware EllieWeb.Graphql.Middleware.Auth
      resolve fn _,_,_ -> {:ok, %Ellie.Settings{}} end
    end
  end

  mutation do
    @desc "Example mutation"
    field :authenticate, non_null(:user_auth) do
      resolve &Authenticate.call/2
    end
  end
end

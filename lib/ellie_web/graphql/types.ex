defmodule EllieWeb.Graphql.Types do
  use Absinthe.Schema.Notation

  enum :theme do
    value :dark, as: "DARK"
    value :light, as: "LIGHT"
  end

  object :settings do
    field :font_size, non_null(:string)
    field :font_family, non_null(:string)
    field :theme, non_null(:theme)
    field :vim_mode, non_null(:boolean)
  end

  object :user do
    field :id, non_null(:uuid)
    field :settings, non_null(:settings)
  end

  object :user_auth do
    field :user, non_null(:user)
    field :token, non_null(:string)
  end
end

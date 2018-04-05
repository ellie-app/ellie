defmodule EllieWeb.Graphql.Types do
  use Absinthe.Schema.Notation
  alias Ellie.Repo

  enum :theme do
    value :dark, as: "DARK"
    value :light, as: "LIGHT"
  end

  object :package do
    field :name, non_null(:name)
    field :version, non_null(:version)
  end

  input_object :package_input do
    field :name, non_null(:name)
    field :version, non_null(:version)
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

  object :revision do
    field :project_id, non_null(:uuid)
    field :revision_number, non_null(:integer)
    field :title, :string
    field :elm_code, non_null(:string)
    field :html_code, non_null(:string)
    field :packages, non_null(list_of(non_null(:package)))
    field :elm_version, non_null(:version)
    field :terms_version, :integer
    field :user, non_null(:user) do
      resolve fn revision, _, _ -> {:ok, Repo.preload(revision, :user).user} end
    end
  end

  input_object :revision_update_input do
    field :title, :string
    field :elm_code, non_null(:string)
    field :html_code, non_null(:string)
    field :packages, non_null(list_of(non_null(:package_input)))
    field :terms_version, non_null(:integer)
  end

  object :user_auth do
    field :user, non_null(:user)
    field :token, non_null(:string)
  end
end

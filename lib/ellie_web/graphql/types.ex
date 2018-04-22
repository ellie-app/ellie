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
    field :docs, non_null(list_of(non_null(:elm_docs_module))) do
      resolve &EllieWeb.Graphql.Resolvers.PackageDocs.call/3
    end
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
    field :terms_version, :integer
  end

  object :revision do
    field :project_id, non_null(:project_id)
    field :revision_number, non_null(:integer)
    field :title, :string
    field :elm_code, non_null(:string)
    field :html_code, non_null(:string)
    field :packages, non_null(list_of(non_null(:package)))
    field :elm_version, non_null(:version)
    field :terms_version, :integer
    field :user, :user do
      resolve fn revision, _, _ -> {:ok, Repo.preload(revision, :user).user} end
    end
  end

  input_object :revision_update_input do
    field :title, :string
    field :elm_code, non_null(:string)
    field :html_code, non_null(:string)
    field :packages, non_null(list_of(non_null(:package_input)))
  end

  object :user_auth do
    field :user, non_null(:user)
    field :token, non_null(:string)
    field :terms_version, non_null(:integer)
  end

  object :position do
    field :line, non_null(:integer)
    field :column, non_null(:integer)
  end

  object :region do
    field :start, non_null(:position)
    field :end, non_null(:position)
  end

  object :compiler_error do
    field :title, non_null(:string)
    field :message, non_null(:string)
    field :region, non_null(:region)
  end

  object :workspace_attached do
    field :packages, non_null(list_of(non_null(:package)))
  end

  object :compile_completed do
    field :error, :elm_error
  end

  union :workspace_update do
    types [:workspace_attached, :compile_completed]
    resolve_type fn
      %{packages: _}, _ -> :workspace_attached
      %{error: _}, _ -> :compile_completed
    end
  end

  object :embed_ready do
    field :error, :elm_error
  end

  object :embed_failed do
    field :message, non_null(:string)
  end

  union :embed_update do
    types [:embed_ready, :embed_failed]
    resolve_type fn
      %{error: _}, _ -> :embed_ready
      %{message: _}, _ -> :embed_failed
    end
  end
end

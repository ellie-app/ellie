defmodule EllieWeb.Graphql.Types.Revision do
  use Absinthe.Schema.Notation

  object :revision do
    field :id, non_null(:project_id)
    field :title, :string
    field :elm_code, non_null(:string)
    field :html_code, non_null(:string)
    field :packages, non_null(list_of(non_null(:elm_package)))
    field :elm_version, non_null(:elm_version)
    field :terms_version, :integer
  end

  input_object :revision_input do
    field :title, :string
    field :elm_code, non_null(:string)
    field :html_code, non_null(:string)
    field :packages, non_null(list_of(non_null(:elm_package_input)))
    field :terms_version, non_null(:integer)
  end
end

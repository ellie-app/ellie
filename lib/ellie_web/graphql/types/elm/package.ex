defmodule EllieWeb.Graphql.Types.Elm.Package do
  alias Elm.Platform
  alias Elm.Package

  use Absinthe.Schema.Notation

  object :elm_package do
    field :name, non_null(:elm_name)
    field :version, non_null(:elm_version)

    field :docs, non_null(list_of(non_null(:elm_docs_module))) do
      resolve fn package, _args, _ctx ->
        case Platform.docs(package) do
          {:ok, modules} -> {:ok, modules}
          :error -> {:error, "Failed to load docs for package"}
        end
      end
    end
  end

  input_object :elm_package_input do
    field :name, non_null(:elm_name)
    field :version, non_null(:elm_version)
  end

  def from_input(input_package) do
    %Package{name: input_package.name, version: input_package.version}
  end
end

defmodule EllieWeb.Graphql.Types.Elm.Docs do
  use Absinthe.Schema.Notation

  object :elm_docs_module do
    field :name, non_null(:string)
    field :comment, non_null(:string)
    field :unions, non_null(list_of(non_null(:elm_docs_union)))
    field :aliases, non_null(list_of(non_null(:elm_docs_alias)))
    field :values, non_null(list_of(non_null(:elm_docs_value)))
    field :binops, non_null(list_of(non_null(:elm_docs_binop)))
  end

  object :elm_docs_union do
    field :name, non_null(:string)
    field :comment, non_null(:string)
    field :args, non_null(list_of(non_null(:string)))

    field :tags, non_null(list_of(non_null(:elm_docs_tag))) do
      resolve fn union, _, _ ->
        {:ok, Enum.map(union.tags, fn {name, args} -> %{name: name, args: args} end)}
      end
    end
  end

  object :elm_docs_tag do
    field :name, non_null(:string)
    field :args, non_null(list_of(non_null(:elm_docs_type)))
  end

  scalar :elm_docs_type, name: "ElmDocsType" do
    serialize fn string -> string end
    parse fn
      %Absinthe.Blueprint.Input.String{value: value} -> {:ok, value}
      %Absinthe.Blueprint.Input.Null{}               -> {:ok, nil}
      _                                              -> :error
    end
  end

  object :elm_docs_alias do
    field :name, non_null(:string)
    field :comment, non_null(:string)
    field :args, non_null(list_of(non_null(:string)))
    field :type, non_null(:elm_docs_type)
  end

  object :elm_docs_value do
    field :name, non_null(:string)
    field :comment, non_null(:string)
    field :type, non_null(:elm_docs_type)
  end

  object :elm_docs_binop do
    field :name, non_null(:string)
    field :comment, non_null(:string)
    field :type, non_null(:elm_docs_type)
    field :associativity, non_null(:elm_docs_associativity)
    field :precedence, non_null(:integer)
  end

  enum :elm_docs_associativity do
    value :right
    value :left
    value :none
  end
end

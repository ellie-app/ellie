defmodule EllieWeb.Graphql.Types.Elm.Docs do
  use Absinthe.Schema.Notation

  defp good_list(type), do: non_null(list_of(non_null(type)))

  object :elm_docs_module do
    field :name, non_null(:string)
    field :comment, non_null(:string)
    field :unions, good_list(:elm_docs_union)
    field :aliases, good_list(:elm_docs_alias)
    field :values, good_list(:elm_docs_value)
    field :binops, good_list(:elm_docs_binop)
  end

  object :elm_docs_union do
    field :name, non_null(:string)
    field :comment, non_null(:string)
    field :args, good_list(:string)
    field :tags, good_list(:elm_docs_tag) do
      resolve fn union, _, _ ->
        {:ok, Map.get(union, "tags", [])}
      end
    end
  end

  object :elm_docs_tag do
    field :name, non_null(:string)
    field :args, good_list(:elm_docs_type)
  end

  scalar :elm_docs_type, name: "ElmDocsType" do
    serialize fn string -> string end
    parse fn
      %Absinthe.Blueprint.Input.String{value: value} -> {:ok, value}
      %Absinthe.Blueprint.Input.Null{} -> {:ok, nil}
      _ ->:error
    end
  end

  object :elm_docs_alias do
    field :name, non_null(:string)
    field :comment, non_null(:string)
    field :args, good_list(:string)
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
    value :right, as: "right"
    value :left, as: "left"
    value :none, as: "non"
  end
end

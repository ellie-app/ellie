defmodule EllieWeb.Graphql.Types.WorkspaceUpdate do
  use Absinthe.Schema.Notation

  object :workspace_attached do
    field :packages, non_null(list_of(non_null(:elm_package)))
  end

  object :compile_completed do
    field :error, :elm_error
  end

  object :workspace_error do
    field :message, non_null(:string)
  end

  union :workspace_update do
    types [:workspace_attached, :compile_completed, :workspace_error]

    resolve_type fn
      %{message: _}, _ -> :workspace_error
      %{packages: _}, _ -> :workspace_attached
      %{error: _}, _ -> :compile_completed
    end
  end
end

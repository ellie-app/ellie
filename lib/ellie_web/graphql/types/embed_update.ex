defmodule EllieWeb.Graphql.Types.EmbedUpdate do
  use Absinthe.Schema.Notation

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

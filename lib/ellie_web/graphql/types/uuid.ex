defmodule EllieWeb.Graphql.Types.Uuid do
  use Absinthe.Schema.Notation

  alias Ecto.UUID

  scalar :uuid, name: "UUID" do
    serialize &UUID.cast!/1
    parse &cast_uuid/1
  end

  defp cast_uuid(%Absinthe.Blueprint.Input.String{value: value}) do
    UUID.cast(value)
  end

  defp cast_uuid(%Absinthe.Blueprint.Input.Null{}) do
    {:ok, nil}
  end

  defp cast_uuid(_) do
    :error
  end
end

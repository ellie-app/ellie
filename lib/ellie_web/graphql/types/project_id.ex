defmodule EllieWeb.Graphql.Types.PrettyId do
  use Absinthe.Schema.Notation
  alias Ellie.Types.PrettyId

  scalar :project_id, name: "PrettyId" do
    serialize &to_string/1
    parse &cast_from_string/1
  end

  defp cast_from_string(%Absinthe.Blueprint.Input.String{value: value}) do
    PrettyId.cast(value)
  end

  defp cast_from_string(%Absinthe.Blueprint.Input.Null{}) do
    {:ok, nil}
  end

  defp cast_from_string(_) do
    :error
  end
end

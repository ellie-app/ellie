defmodule EllieWeb.Graphql.Types.ProjectId do
  use Absinthe.Schema.Notation
  alias Ellie.ProjectId

  scalar :project_id, name: "ProjectId" do
    serialize &ProjectId.to_string/1
    parse &cast_from_string/1
  end

  defp cast_from_string(%Absinthe.Blueprint.Input.String{value: value}) do
    {:ok, ProjectId.from_string(value)}
  end

  defp cast_from_string(%Absinthe.Blueprint.Input.Null{}) do
    {:ok, nil}
  end

  defp cast_from_string(_) do
    :error
  end
end

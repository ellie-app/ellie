defmodule EllieWeb.Graphql.Types.Name do
  use Absinthe.Schema.Notation

  scalar :name, name: "Name" do
    serialize &serialize_to_string/1
    parse &cast_from_string/1
  end

  defp serialize_to_string(%Ellie.Elm.Name{user: user, project: project}) do
    user <> "/" <> project
  end

  defp cast_from_string(%Absinthe.Blueprint.Input.String{value: value}) do
    with [user, project] <- String.split(value, "/") do
      {:ok, %Ellie.Elm.Name{user: user, project: project}}
    else
      _ -> :error
    end
  end

  defp cast_from_string(%Absinthe.Blueprint.Input.Null{}) do
    {:ok, nil}
  end

  defp cast_from_string(_) do
    :error
  end
end

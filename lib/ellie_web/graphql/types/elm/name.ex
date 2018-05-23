defmodule EllieWeb.Graphql.Types.Elm.Name do
  use Absinthe.Schema.Notation

  scalar :elm_name, name: "ElmName" do
    serialize &serialize_to_string/1
    parse &cast_from_string/1
  end

  defp serialize_to_string(%Elm.Name{user: user, project: project}) do
    user <> "/" <> project
  end

  defp cast_from_string(%Absinthe.Blueprint.Input.String{value: value}) do
    with [user, project] <- String.split(value, "/") do
      {:ok, %Elm.Name{user: user, project: project}}
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

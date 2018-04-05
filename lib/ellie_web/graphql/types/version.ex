defmodule EllieWeb.Graphql.Types.Version do
  use Absinthe.Schema.Notation

  scalar :version, name: "Version" do
    serialize &serialize_to_string/1
    parse &cast_from_string/1
  end

  defp serialize_to_string(%Ellie.Elm.Version{major: major, minor: minor, patch: patch}) do
    Integer.to_string(major) <> "." <> Integer.to_string(minor) <> "." <> Integer.to_string(patch)
  end

  defp cast_from_string(%Absinthe.Blueprint.Input.String{value: value}) do
    with [major, minor, patch] <- String.split(value, ".") do
      {:ok, %Ellie.Elm.Version{major: String.to_integer(major),
                               minor: String.to_integer(minor),
                               patch: String.to_integer(patch)}}
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

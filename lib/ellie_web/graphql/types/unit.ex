defmodule EllieWeb.Graphql.Types.Unit do
  use Absinthe.Schema.Notation

  scalar :unit, name: "Unit" do
    serialize &serialize_unit/1
    parse &parse_unit/1
  end

  defp parse_unit(%Absinthe.Blueprint.Input.Null{}), do: {:ok, nil}
  defp parse_unit(_), do: {:ok, :unit}

  defp serialize_unit(:unit), do: "()"
end

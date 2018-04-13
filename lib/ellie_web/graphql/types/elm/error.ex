defmodule EllieWeb.Graphql.Types.Elm.Error do
  use Absinthe.Schema.Notation
  require IEx

  defp format_message(_, %{source: source}) do
    stuff =
      source
        |> Map.get("message", [])
        |> Enum.map(fn
            string when is_binary(string) ->
              %{string: string, style: nil}
            a ->
              %{
                string: Map.get(a, "string"),
                style: %{
                  bold: Map.get(a, "bold"),
                  color: Map.get(a, "color"),
                  underline: Map.get(a, "underline")
                }
              }
          end)
    {:ok, stuff}
  end

  union :elm_error do
    types [:elm_error_general_problem, :elm_error_module_problems]
    resolve_type fn value, _ ->
      case value do
        %{"type" => "error"} -> :elm_error_general_problem
        %{"type" => "compile-errors"} -> :elm_error_module_problems
      end
    end
  end

  object :elm_error_general_problem do
    field :path, non_null(:string)
    field :title, non_null(:string)
    field :message, non_null(list_of(non_null(:elm_error_chunk))) do
      resolve &format_message/2
    end
  end

  object :elm_error_module_problems do
    field :errors, non_null(list_of(non_null(:elm_error_bad_module)))
  end

  object :elm_error_bad_module do
    field :path, non_null(:string)
    field :name, non_null(:string)
    field :problems, non_null(list_of(non_null(:elm_error_problem)))
  end

  object :elm_error_problem do
    field :title, non_null(:string)
    field :region, non_null(:elm_error_region)
    field :message, non_null(list_of(non_null(:elm_error_chunk))) do
      resolve &format_message/2
    end
  end

  object :elm_error_chunk do
    field :string, non_null(:string)
    field :style, :elm_error_style
  end

  object :elm_error_style do
    field :bold, non_null(:boolean)
    field :underline, non_null(:boolean)
    field :color, :elm_error_color
  end

  object :elm_error_region do
    field :start, non_null(:elm_error_position)
    field :end, non_null(:elm_error_position)
  end

  object :elm_error_position do
    field :line, non_null(:integer)
    field :column, non_null(:integer)
  end

  enum :elm_error_color do
    value :red, as: "red"
    value :vivid_red, as: "RED"
    value :magenta, as: "magenta"
    value :vivid_magenta, as: "MAGENTA"
    value :yellow, as: "yellow"
    value :vivid_yellow, as: "YELLOW"
    value :green, as: "green"
    value :vivid_green, as: "GREEN"
    value :cyan, as: "cyan"
    value :vivid_cyan, as: "CYAN"
    value :blue, as: "blue"
    value :vivid_blue, as: "BLUE"
    value :white, as: "white"
    value :vivid_white, as: "WHITE"
    value :black, as: "black"
    value :vivid_black, as: "BLACK"
  end
end

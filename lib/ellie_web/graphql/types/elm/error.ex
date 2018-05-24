defmodule EllieWeb.Graphql.Types.Elm.Error do
  use Absinthe.Schema.Notation

  defp format_message(message) do
    Enum.map(message, fn
      {:unstyled, string} -> %{string: string, style: nil}
      {:styled, style, string} -> %{string: string, style: style}
    end)
  end

  union :elm_error do
    types [:elm_error_general_problem, :elm_error_module_problems]
    resolve_type fn
      {:general_problem, _}, _ -> :elm_error_general_problem
      {:module_problems, _}, _ -> :elm_error_module_problems
    end
  end

  object :elm_error_general_problem do
    field :path, :string do
      resolve fn {_, a}, _, _ -> {:ok, a.path} end
    end
    field :title, non_null(:string) do
      resolve fn {_, a}, _, _ -> {:ok, a.title} end
    end
    field :message, non_null(list_of(non_null(:elm_error_chunk))) do
      resolve fn {_, a}, _, _ ->
        {:ok, format_message(a.message)}
      end
    end
  end

  object :elm_error_module_problems do
    field :errors, non_null(list_of(non_null(:elm_error_bad_module))) do
      resolve fn {_, a}, _, _ -> {:ok, a} end
    end
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
      resolve fn problem, _args, _ctx -> {:ok, format_message(problem.message)} end
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
    value :red
    value :vivid_red
    value :magenta
    value :vivid_magenta
    value :yellow
    value :vivid_yellow
    value :green
    value :vivid_green
    value :cyan
    value :vivid_cyan
    value :blue
    value :vivid_blue
    value :white
    value :vivid_white
    value :black
    value :vivid_black
  end
end

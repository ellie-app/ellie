defmodule Ellie.Elm.Platform.Parser do
  use Combine, parsers: [:text]

  def module_name(text) do
    text
    |> run(module_name_parser())
    |> with_default("Main")
  end

  def module_path(text) do
    String.replace(module_name(text), ".", "/") <> ".elm"
  end

  defp module_name_parser() do
    choice([
      ignore(string("module")),
      ignore(string("port")) |> ignore(spaces()) |> ignore(string("module")),
      ignore(string("effect")) |> ignore(spaces()) |> ignore(string("module")),
    ])
    |> ignore(spaces())
    |> map(sep_by1(word_of(~r/[A-Z][a-zA-Z0-9_]+/), char(".")), &Enum.join(&1, "."))
  end

  defp run(text, parser) do
    case Combine.parse(text, parser) do
      {:error, _} -> nil
      stuff -> List.first(stuff)
    end
  end

  defp with_default(nil, a), do: a
  defp with_default(a, _a), do: a
end

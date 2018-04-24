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

  def error_0_19_0(input) do
    input
    |> String.split("\n")
    |> Enum.flat_map(fn line ->
      if String.starts_with?(line, "{") do
        [Poison.decode!(line)]
      else
        []
      end
    end)
    |> List.first()
  end

  def error_0_18_0(entry, input) do
    errors =
      input
      |> String.split("\n")
      |> Enum.flat_map(fn line ->
        if String.starts_with?(line, "[") do
          line
          |> Poison.decode!()
          |> List.foldr(%{}, fn error, by_source ->
            case Map.get(error, "type") do
              "error" ->
                file = Map.get(error, "file", "src/Main.elm")
                problem = %{
                  "title" => Map.get(error, "tag"),
                  "region" => Map.get(error, "subregion") || Map.get(error, "region"),
                  "message" => [Map.get(error, "overview"), "\n\n", Map.get(error, "details")]
                }
                by_source
                |> Map.put_new(file, [])
                |> Map.update!(file, fn problems -> [problem | problems] end)
              _ ->
                by_source
            end
          end)
          |> Enum.map(fn {file, problems} ->
            %{
              "name" => path_to_module(file),
              "path" => file,
              "problems" => problems
            }
          end)
        else
          []
        end
      end)
    if Enum.empty?(errors) do
      %{
        "type" => "error",
        "path" => entry,
        "title" => "Compiler Error",
        "message" => [input]
      }
    else
      %{
        "type" => "compile-errors",
        "errors" => errors
      }
    end
  end

  defp path_to_module(input) do
    case String.split(input, "src/") do
      [_, local_name] ->
        local_name
        |> String.trim_trailing(".elm")
        |> String.replace("/", ".")
      _ ->
        "Main"
    end
  end
end

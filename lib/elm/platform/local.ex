defmodule Elm.Platform.Local do
  alias Elm.Platform.Local18
  alias Elm.Platform.Local19
  alias Elm.Version
  alias Elm.Platform.Parser

  @behaviour Elm.Platform

  def setup(root, %Version{major: 0, minor: 18, patch: 0}), do: Local18.setup(root)
  def setup(root, %Version{major: 0, minor: 19, patch: 0}), do: Local19.setup(root)
  def setup(root, %Version{major: 0, minor: 19, patch: 1}), do: Local19.setup(root)
  def setup(_root, _version), do: :error

  def compile(root, options) do
    project = Keyword.fetch!(options, :project)

    case project.elm_version do
      %Version{major: 0, minor: 18, patch: 0} -> Local18.compile(root, options)
      %Version{major: 0, minor: 19, patch: 0} -> Local19.compile(root, options)
      %Version{major: 0, minor: 19, patch: 1} -> Local19.compile(root, options)
      _ -> :error
    end
  end

  def format(code, %Version{major: 0, minor: 18, patch: 0}), do: Local18.format(code)
  def format(code, %Version{major: 0, minor: 19, patch: 0}), do: Local19.format(code)
  def format(code, %Version{major: 0, minor: 19, patch: 1}), do: Local19.format(code)
  def format(_code, _version), do: :error

  defp dets_path, do: String.to_charlist(Path.join([:code.priv_dir(:ellie), "docs_table"]))

  defp render_json_error({:field, field, e}), do: "At field #{field}: #{render_json_error(e)}"
  defp render_json_error({:index, index, e}), do: "At index #{index}: #{render_json_error(e)}"

  defp render_json_error({:one_of, es}),
    do: es |> Enum.map(&render_json_error/1) |> Enum.join(", ")

  defp render_json_error({:failure, message, _value}), do: message
  defp render_json_error(_), do: "IDK"

  def docs(package) do
    {:ok, table} = :dets.open_file(:docs_table, file: dets_path)

    case :dets.lookup(table, package) do
      [{^package, modules}] ->
        :dets.close(table)
        {:ok, modules}

      _ ->
        with {:ok, %HTTPoison.Response{status_code: 200, body: body}} <-
               HTTPoison.get(docs_url(package)),
             {:ok, modules} <- Parser.docs_json(body) do
          :dets.insert(table, {package, modules})
          :dets.close(table)
          {:ok, modules}
        else
          {:error, json_error} ->
            :error

          error ->
            :error
        end
    end
  end

  def search() do
    url = "https://package.elm-lang.org/search.json"

    with {:ok, %HTTPoison.Response{status_code: 200, body: body}} <- HTTPoison.get(url),
         {:ok, searchables} <- Parser.searchables_json(body) do
      {:ok, searchables}
    else
      _ -> :error
    end
  end

  defp docs_url(package) do
    "https://package.elm-lang.org/packages/" <>
      package.name.user <>
      "/" <> package.name.project <> "/" <> Version.to_string(package.version) <> "/docs.json"
  end
end

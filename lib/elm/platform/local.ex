defmodule Elm.Platform.Local do
  alias Elm.Platform.Local18
  alias Elm.Platform.Local19
  alias Elm.Version
  alias Elm.Platform.Parser

  @behaviour Elm.Platform

  def setup(root, %Version{major: 0, minor: 18, patch: 0}), do: Local18.setup(root)
  def setup(root, %Version{major: 0, minor: 19, patch: 0}), do: Local19.setup(root)
  def setup(_root, _version), do: :error

  def compile(root, options) do
    project = Keyword.fetch!(options, :project)
    case project.elm_version do
      %Version{major: 0, minor: 19, patch: 0} -> Local19.compile(root, options)
      %Version{major: 0, minor: 18, patch: 0} -> Local18.compile(root, options)
      _ -> :error
    end
  end

  def format(code, %Version{major: 0, minor: 18, patch: 0}), do: Local18.format(code)
  def format(code, %Version{major: 0, minor: 19, patch: 0}), do: Local19.format(code)
  def format(_code, _version), do: :error

  @dets_path String.to_charlist(Path.join([:code.priv_dir(:ellie), "docs_table"]))

  def docs(package) do
    {:ok, table} = :dets.open_file(:docs_table, file: @dets_path)
    case :dets.lookup(table, package) do
      [{^package, modules}] ->
        :dets.close(table)
        {:ok, modules}
      _ ->
        IO.inspect(docs_url(package))
        with {:ok, %HTTPoison.Response{status_code: 200, body: body}} <- HTTPoison.get(docs_url(package)),
             {:ok, modules} <- Parser.docs_json(body)
        do
          :dets.insert(table, {package, modules})
          :dets.close(table)
          {:ok, modules}
        else
          error ->
            IO.inspect(error)
            :error
        end
    end
  end

  def search() do
    url = package_site() <> "/search.json"
    with {:ok, %HTTPoison.Response{status_code: 200, body: body}} <- HTTPoison.get(url),
         {:ok, searchables} <- Parser.searchables_json(body)
    do
      {:ok, searchables}
    else
      _ -> :error
    end
  end

  defp docs_url(package) do
    package_site() <>
      "/packages/" <>
      package.name.user <>
      "/" <>
      package.name.project <>
      "/" <>
      Version.to_string(package.version) <>
      "/docs.json"
  end

  defp package_site() do
    default = "http://package.elm-lang.org"
    config = Application.get_env(:ellie, Elm, [])
    case Keyword.get(config, :package_site, default) do
      {:system, variable} when is_binary(variable) -> Map.get(System.get_env(), variable, default)
      value when is_binary(value) -> value
      _ -> default
    end
  end
end

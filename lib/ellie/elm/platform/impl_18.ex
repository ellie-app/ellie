defmodule Ellie.Elm.Platform.Impl18 do
  alias Ellie.Elm.Name
  alias Ellie.Elm.Version
  alias Ellie.Elm.Package
  alias Ellie.Helpers.EnumHelpers
  alias Ellie.Elm.Platform.Parser
  use GenServer

  @bin_path Path.expand("../../../../priv/bin/0.18.0", __DIR__)

  @behaviour Ellie.Elm.Platform

  def setup(root) do
    case GenServer.call(__MODULE__, {:elm_package, :init, root}) do
      :ok ->
        root
        |> read_project()
        |> Map.put("source-directories", ["src"])
        |> write_project(root)
        File.mkdir_p!(Path.join(root, "src"))
        :ok
      error ->
        error
    end
  end

  def compile(options) do
    %{root: root, entry: entry, output: output} = Enum.into(options, %{})
    GenServer.call(__MODULE__, {:elm_make, root, entry, output}, :infinity)
  end

  def format(code) do
    binary = Path.join(@bin_path, "elm-format")
    args = ["--stdin"]
    options = [in: code, out: :string, err: :string]
    result = Porcelain.exec(binary, args, options)
    case result do
      %Porcelain.Result{err: "", out: out, status: 0} ->
        {:ok, out}
      _ ->
        {:error, "elm-format failed to run"}
    end
  end

  def install(root, packages) do
    root
    |> read_project()
    |> Map.put("dependencies", make_deps_map(packages))
    |> write_project(root)
    File.rm_rf!(Path.join(root, "elm-stuff/exact-dependencies.json"))
    :ok
  end

  def dependencies(root) do
    project = read_project(root)
    decode_deps_map(Map.get(project, "dependencies"))
  end

  defp write_project(project, path) do
    File.write!(Path.join(path, "elm-package.json"), Poison.encode!(project))
  end

  defp read_project(root) do
    root
    |> Path.join("elm-package.json")
    |> File.read!()
    |> Poison.decode!()
  end

  defp make_deps_map(deps) do
    Enum.reduce(deps, %{}, fn next, deps ->
      version_string = Version.to_string(next.version)
      Map.put(deps, Name.to_string(next.name), "#{version_string} <= v <= #{version_string}")
    end)
  end

  defp decode_deps_map(map) do
    stuff =
      map
      |> Map.to_list()
      |> EnumHelpers.traverse_result(fn {key, value} ->
        with {:ok, name} <- Name.from_string(key),
          [version_string, _, "v", _, _] <- String.split(value, " "),
          {:ok, version} <- Version.from_string(version_string)
        do
          {:ok, %Package{version: version, name: name}}
        else
          error -> error
        end
      end)
    case stuff do
      {:ok, ps} -> {:ok, MapSet.new(ps)}
      error -> error
    end
  end

  ## SERVER

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    {:ok, nil}
  end

  def handle_call({:elm_package, :init, root}, _from, state) do
    binary = Path.join(@bin_path, "elm-package")
    args = ["install", "--yes"]
    options = [out: :string, err: :string, dir: root]
    result = Porcelain.exec(binary, args, options)
    case result do
      %Porcelain.Result{status: 0} ->
        {:reply, :ok, state}
      _ ->
        {:reply, {:error, "failed to set up Elm 0.18.0 project"}, state}
    end
  end

  def handle_call({:elm_make, root, entry, output}, _from, state) do
    binary = Path.join(@bin_path, "elm-make")
    args = [entry, "--report", "json", "--yes", "--debug", "--output", output]
    options = [out: :string, err: :string, dir: root]
    result = Porcelain.exec(binary, args, options)
    case result do
      %Porcelain.Result{status: 0} ->
        {:reply, {:ok, nil}, state}
      %Porcelain.Result{status: 1, out: out, err: ""} ->
        {:reply, {:ok, Parser.error_0_18_0(entry, out)}, state}
      %Porcelain.Result{err: err} ->
        {:reply, {:ok, Parser.error_0_18_0(entry, err)}, state}
      {:error, reason} ->
        {:reply, {:ok, Parser.error_0_18_0(entry, reason)}, state}
    end
  end
end

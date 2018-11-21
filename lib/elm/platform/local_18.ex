defmodule Elm.Platform.Local18 do
  alias Data.Result
  alias Elm.Name
  alias Elm.Version
  alias Elm.Package
  alias Elm.Project
  alias Elm.Error
  alias Elm.Platform.Parser
  use GenServer

  @base_path Path.join(:code.priv_dir(:ellie), "bin/0.18.0")

  # Callbacks

  @spec setup(Path.t()) :: {:ok, Project.t()} | :error
  def setup(root) do
    case GenServer.call(__MODULE__, {:elm_package, :init, root}, :infinity) do
      :ok ->
        project_json = read_project!(root)
        updated = Map.put(project_json, "source-directories", ["src"])
        write_project!(updated, root)
        File.mkdir_p!(Path.join(root, "src"))

        case decode_deps_map!(Map.fetch!(project_json, "dependencies")) do
          {:ok, dependencies} ->
            project = %Project{
              elm_version: Version.create(0, 18, 0),
              dependencies: dependencies
            }

            {:ok, project}

          _error ->
            :error
        end

      :error ->
        :error
    end
  end

  @spec compile(Path.t(), source: String.t(), project: Project.t(), output: Path.t()) ::
          {:ok, Error.t() | nil} | :error
  def compile(root, options) do
    source = Keyword.fetch!(options, :source)
    project = Keyword.fetch!(options, :project)
    output = Keyword.fetch!(options, :output)

    if project_exists?(root) do
      root
      |> read_project!()
      |> Map.put("dependencies", make_deps_map(project.dependencies))
      |> write_project!(root)

      File.rm_rf!(Path.join([root, "elm-stuff", "exact-dependencies.json"]))

      File.rm_rf!(Path.join(root, "src"))

      entry = Path.join("src", Parser.module_path(source))

      File.mkdir_p!(Path.join([root, Path.dirname(entry)]))
      File.write!(Path.join([root, entry]), source)

      error = GenServer.call(__MODULE__, {:elm_make, root, entry, output}, :infinity)
      {:ok, error}
    else
      :error
    end
  end

  @spec format(String.t()) :: {:ok, String.t()} | :error
  def format(code) do
    binary = Path.join(@base_path, "elm-format")
    args = ["--stdin"]
    options = [in: code, out: :string, err: :string]
    result = Porcelain.exec(binary, args, options)

    case result do
      %Porcelain.Result{err: "", out: out, status: 0} ->
        {:ok, out}

      _ ->
        :error
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
    binary = Path.join(@base_path, "elm-package")
    args = ["--num", "1", binary, "install", "--yes"]
    options = [out: :string, err: :string, dir: root]
    result = Porcelain.exec("sysconfcpus", args, options)

    case result do
      %Porcelain.Result{status: 0} ->
        {:reply, :ok, state}

      _ ->
        {:reply, :error, state}
    end
  end

  def handle_call({:elm_make, root, entry, output}, _from, state) do
    binary = Path.join(@base_path, "elm-make")

    args = [
      "--num",
      "1",
      binary,
      entry,
      "--report",
      "json",
      "--yes",
      "--debug",
      "--output",
      output
    ]

    options = [out: :string, err: :string, dir: root]
    result = Porcelain.exec("sysconfcpus", args, options)

    case result do
      %Porcelain.Result{status: 0} ->
        {:reply, nil, state}

      %Porcelain.Result{status: 1, out: out, err: ""} ->
        {:reply, parse_error(entry, out), state}

      %Porcelain.Result{err: err} ->
        {:reply, parse_error(entry, err), state}

      {:error, reason} ->
        {:reply, parse_error(entry, reason), state}
    end
  end

  # Helpers

  defp project_exists?(root) do
    root
    |> Path.join("elm-package.json")
    |> File.exists?()
  end

  defp write_project!(project, path) do
    path
    |> Path.join("elm-package.json")
    |> File.write!(Poison.encode!(project))
  end

  defp read_project!(root) do
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

  defp decode_deps_map!(map) do
    map
    |> Map.to_list()
    |> Result.traverse(fn {key, value} ->
      with {:ok, name} <- Name.from_string(key),
           [version_string, _, "v", _, _] <- String.split(value, " "),
           {:ok, version} <- Version.from_string(version_string) do
        {:ok, %Package{version: version, name: name}}
      else
        :error -> {:error, nil}
        error -> error
      end
    end)
    |> Result.map(&MapSet.new/1)
  end

  def parse_error(entry, input) do
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

                top_key =
                  if is_nil(Map.get(error, "subregion")) do
                    "region"
                  else
                    "subregion"
                  end

                region = %Error.Region{
                  start: %Error.Position{
                    line: get_in(error, [top_key, "start", "line"]),
                    column: get_in(error, [top_key, "start", "column"])
                  },
                  end: %Error.Position{
                    line: get_in(error, [top_key, "end", "line"]),
                    column: get_in(error, [top_key, "end", "column"])
                  }
                }

                problem = %Error.Problem{
                  title: Map.fetch!(error, "tag"),
                  region: region,
                  message: [
                    {:unstyled, Map.get(error, "overview", "")},
                    {:unstyled, "\n\n"},
                    {:unstyled, Map.get(error, "details", "")}
                  ]
                }

                by_source
                |> Map.put_new(file, [])
                |> Map.update!(file, fn problems -> [problem | problems] end)

              _ ->
                by_source
            end
          end)
          |> Enum.map(fn {file, problems} ->
            %Error.BadModule{
              name: path_to_module(file),
              path: file,
              problems: problems
            }
          end)
        else
          []
        end
      end)

    if Enum.empty?(errors) do
      gp = %Error.GeneralProblem{
        path: entry,
        title: "Compiler Error",
        message: [{:unstyled, input}]
      }

      {:general_problem, gp}
    else
      {:module_problems, errors}
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

defmodule Elm.Platform.Local19 do
  alias Elm.Project
  alias Elm.Name
  alias Elm.Package
  alias Elm.Platform.Parser
  alias Elm.Version
  require Logger

  @spec setup(Path.t) :: {:ok, Project.t} | :error
  def setup(root) do
    with :ok <- elm_init(root) do
      File.mkdir_p!(Path.join(root, "src"))
      project_json = read_project!(root)
      project = %Project{
        elm_version: Version.create(0, 19, 0),
        dependencies: decode_deps_map!(Map.fetch!(project_json, "dependencies"))
      }
      {:ok, project}
    else
      _ ->
        :error
    end
  end

  @spec compile(Path.t, [source: String.t, project: Project.t, output: Path.t]) :: {:ok, Error.t | nil} | :error
  def compile(root, options) do
    source = Keyword.fetch!(options, :source)
    project = Keyword.fetch!(options, :project)
    output = Keyword.fetch!(options, :output)

    project_json = read_project!(root)
    current_deps = decode_deps_map!(Map.get(project_json, "dependencies", %{}))

    install_result =
      if not MapSet.equal?(current_deps, project.dependencies) do
        default_project()
        |> Map.put("dependencies", make_deps_map(project.dependencies))
        |> write_project!(root)
        install_transitive_deps(root)
      else
        :ok
      end

    case install_result do
      :ok ->
        File.rm_rf!(Path.join(root, "src"))
        entry = Path.join("src", Parser.module_path(source))
        File.mkdir_p!(Path.join(root, Path.dirname(entry)))
        File.write!(Path.join(root, entry), source)

        binary = Application.app_dir(:ellie, "priv/bin/0.19.0/elm")
        args = ["--num", "1", binary, "make", entry, "--debug", "--output", output, "--report", "json"]

        options = [dir: root, out: :string, err: :string]
        result = Porcelain.exec("sysconfcpus", args, options)
        Logger.info("elm make\nexit: #{result.status}\nstdout: #{result.out}\nstderr: #{result.err}\n")
        case result do
          %Porcelain.Result{err: err, status: 0} ->
            {:ok, parse_error(err)}
          %Porcelain.Result{err: err, status: 1} ->
            {:ok, parse_error(err)}
          _ ->
            :error
        end
      :error ->
        :error
      end
  end

  @spec format(String.t) :: {:ok, String.t} | :error
  def format(code) do
    binary = Application.app_dir(:ellie, "priv/bin/0.19.0/elm-format")
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

  # Helpers

  defp install_transitive_deps(root) do
    binary = Application.app_dir(:ellie, "priv/bin/0.19.0/elm")
    args = ["--num", "1", binary, "make", "--report", "json"]
    options = [dir: root, out: :string, err: :string]
    result = Porcelain.exec("sysconfcpus", args, options)
    Logger.info("elm make\nexit: #{result.status}\nstdout: #{result.out}\nstderr: #{result.err}\n")
    case result do
      %Porcelain.Result{err: err, status: 1} ->
        case explore_dependency_error(err) do
          {:ok, :missing_required, name} ->
            original_elm_json =
              root
              |> Path.join("elm.json.original")
              |> File.read!()
              |> Poison.decode!()
            case get_in(original_elm_json, ["do-not-edit-this-by-hand", "transitive-dependencies", Name.to_string(name)]) do
              version_string when not is_nil(version_string) ->
                root
                |> read_project!()
                |> put_in(["do-not-edit-this-by-hand", "transitive-dependencies", Name.to_string(name)], version_string)
                |> write_project!(root)
                install_transitive_deps(root)
              nil ->
                :ok
            end
          {:ok, :missing_transitives, packages} ->
            project_json = read_project!(root)
            packages
            |> Enum.reduce(project_json, fn package, pj ->
                put_in(pj, ["do-not-edit-this-by-hand", "transitive-dependencies", Name.to_string(package.name)], Version.to_string(package.version))
              end)
            |> write_project!(root)
            install_transitive_deps(root)
          {:ok, :nothing_missing} ->
            :ok
          {:error, _reason} ->
            :error
        end
      _ ->
        :error
    end
  end

  defp explore_dependency_error(json_string) do
    case Poison.decode(json_string) do
      {:ok, error_data} ->
        case {Map.get(error_data, "type"), Map.get(error_data, "title")} do
          {"error", "MISSING DEPENDENCY"} ->
            case Map.get(error_data, "message", []) do
              [_first, %{"string" => "elm install " <> package_name_string}, _third] ->
                case Name.from_string(package_name_string) do
                  {:ok, name} -> {:ok, :missing_required, name}
                  :error -> {:error, "Could not parse package name #{package_name_string}."}
                end
              _message ->
                {:error, "Could not understand error message for MISSING DEPENDENCY error."}
            end
          {"error", "MISSING DEPENDENCIES"} ->
            missing_packages =
              case Map.get(error_data, "message", []) do
                [_first, %{"string" => "\"" <> stuff}, _third, _fourth, _fifth] ->
                  stuff
                  |> String.split("\n")
                  |> Enum.map(fn string -> String.trim(String.trim(string), "\"") end)
                  |> Enum.flat_map(fn stuff ->
                    with [name_string, version_string] <- String.split(stuff, "\": \""),
                         {:ok, name} <- Name.from_string(name_string),
                         {:ok, version} <- Version.from_string(version_string)
                    do
                      [%Package{name: name, version: version}]
                    else
                      _stuff -> []
                    end
                  end)
                _other_message ->
                  []
              end
            if Enum.empty?(missing_packages) do
              {:error, "Could not understand error message for MISSING DEPENDENCIES error."}
            else
              {:ok, :missing_transitives, missing_packages}
            end
          {"error", "NO INPUT"} ->
            {:ok, :nothing_missing}
          {"error", other_title} ->
            {:error, "Unexpected error type: #{other_title}."}
          _other_error ->
            {:error, "Unexpected error type."}
        end
    end
  end

  defp elm_init(root) do
    binary = Application.app_dir(:ellie, "priv/bin/0.19.0/elm")
    args = ["init"]
    options = [out: :string, err: :string, dir: root, in: "Y"]
    result = Porcelain.exec(binary, args, options)
    Logger.info("elm init\nexit: #{inspect result}\n")
    case result do
      %Porcelain.Result{status: 0} ->
        with :ok <- File.rm(Path.join(root, "src/Main.elm")),
             :ok <- File.cp(Path.join(root, "elm.json"), Path.join(root, "elm.json.original"))
        do
          :ok
        else
          {:error, reason} ->
            Sentry.capture_message "elm init filesystem error",
              extra: %{
                reason: reason,
                elm_version: "0.19.0"
              }
            {:error, reason}
        end
      %Porcelain.Result{status: other} ->
        Sentry.capture_message "elm init process error",
          extra: %{
            stderr: result.err,
            stdout: result.out,
            status: result.status,
            elm_version: "0.19.0"
          }
        {:error, "init exited with code #{other}"}
      {:error, reason} ->
        Sentry.capture_message "elm init startup error",
          extra: %{reason: reason, elm_version: "0.19.0"}
        {:error, reason}
    end
  end

  defp decode_deps_map!(map) do
    map
    |> Map.to_list()
    |> Enum.map(fn {key, value} ->
      {:ok, name} = Name.from_string(key)
      {:ok, version} = Version.from_string(value)
      %Package{version: version, name: name}
    end)
    |> MapSet.new()
  end

  defp make_deps_map(deps) do
    Enum.reduce(deps, %{}, fn next, deps ->
      Map.put(deps, Name.to_string(next.name), Version.to_string(next.version))
    end)
  end

  defp read_project!(root) do
    root
    |> Path.join("elm.json")
    |> File.read!()
    |> Poison.decode!()
  end

  defp write_project!(project_json, root) do
    root
    |> Path.join("elm.json")
    |> File.write!(Poison.encode!(project_json))
  end

  defp default_project() do
    %{
      "type" => "application",
      "elm-version" => "0.19.0",
      "source-directories" => ["src"],
      "dependencies" => %{},
      "test-dependencies" => %{},
      "do-not-edit-this-by-hand" => %{
        "transitive-dependencies" => %{}
      }
    }
  end

  defp parse_error(input) do
    input
    |> String.split("\n")
    |> Enum.flat_map(fn line ->
      if String.starts_with?(line, "{") do
        data = Poison.decode!(line)
        case Map.fetch!(data, "type") do
          "compile-errors" ->
            modules =
              data
              |> Map.fetch!("errors")
              |> Enum.map(fn a ->
                %Elm.Error.BadModule{
                  path: Map.fetch!(a, "path"),
                  name: Map.fetch!(a, "name"),
                  problems: Enum.map(Map.fetch!(a, "problems"), fn b ->
                    %Elm.Error.Problem{
                      title: Map.fetch!(b, "title"),
                      region: %Elm.Error.Region{
                        start: %Elm.Error.Position{
                          line: get_in(b, ["region", "start", "line"]),
                          column: get_in(b, ["region", "start", "column"])
                        },
                        end: %Elm.Error.Position{
                          line: get_in(b, ["region", "end", "line"]),
                          column: get_in(b, ["region", "end", "column"])
                        }
                      },
                      message: Enum.map(Map.fetch!(b, "message"), &parse_chunk/1)
                    }
                  end)
                }
              end)
            [{:module_problems, modules}]
          "error" ->
            problem =
              %Elm.Error.GeneralProblem{
                path: Map.fetch!(data, "path"),
                title: Map.fetch!(data, "title"),
                message: Enum.map(Map.fetch!(data, "message"), &parse_chunk/1)
              }
            [{:general_problem, problem}]
        end
      else
        []
      end
    end)
    |> List.first()
  end

  defp parse_chunk(string) when is_binary(string) do
    {:unstyled, string}
  end

  defp parse_chunk(%{"bold" => bold, "underline" => underline, "color" => color, "string" => string}) do
    style = %Elm.Error.Style{
      bold: bold,
      underline: underline,
      color:
        if is_nil(color) do
          nil
        else
          Elm.Error.Color.from_string(color)
        end
    }
    {:styled, style, string}
  end
end

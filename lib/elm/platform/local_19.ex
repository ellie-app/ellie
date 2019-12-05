defmodule Elm.Platform.Local19 do
  alias Elm.Project
  alias Elm.Name
  alias Elm.Package
  alias Elm.Platform.Parser
  alias Elm.Version
  require Logger

  @elm_json_binary System.cmd("which", ["elm-json"]) |> elem(0) |> String.trim()

  @spec setup(Path.t()) :: {:ok, Project.t()} | :error
  def setup(root) do
    with :ok <- elm_init(root) do
      File.mkdir_p!(Path.join(root, "src"))
      project_json = read_project!(root)

      project = %Project{
        elm_version: Version.create(0, 19, 1),
        dependencies:
          project_json
          |> Map.get("dependencies", %{})
          |> Map.get("direct", %{})
          |> decode_deps_map!()
      }

      {:ok, project}
    else
      _ ->
        :error
    end
  end

  @spec compile(Path.t(), source: String.t(), project: Project.t(), output: Path.t()) ::
          {:ok, Error.t() | nil} | :error
  def compile(root, options) do
    source = Keyword.fetch!(options, :source)
    project = Keyword.fetch!(options, :project)
    output = Keyword.fetch!(options, :output)

    project_json = read_project!(root)

    current_deps =
      project_json
      |> Map.get("dependencies", %{})
      |> Map.get("direct", %{})
      |> decode_deps_map!()

    install_result =
      case MapSet.difference(project.dependencies, current_deps) |> MapSet.to_list() do
        [] ->
          :ok

        missing_deps ->
          install_missing_deps(root, missing_deps)
      end

    case install_result do
      :ok ->
        File.rm_rf!(Path.join(root, "src"))
        entry = Path.join("src", Parser.module_path(source))
        File.mkdir_p!(Path.join(root, Path.dirname(entry)))
        File.write!(Path.join(root, entry), source)

        binary = Path.join(base_path(), "elm")

        args = [
          "--num",
          "1",
          binary,
          "make",
          entry,
          "--debug",
          "--output",
          output,
          "--report",
          "json"
        ]

        options = [dir: root, out: :string, err: :string]
        result = Porcelain.exec("sysconfcpus", args, options)

        Logger.info(
          "elm make\nexit: #{result.status}\nstdout: #{result.out}\nstderr: #{result.err}\n"
        )

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

  @spec format(String.t()) :: {:ok, String.t()} | :error
  def format(code) do
    binary = Path.join(base_path(), "elm-format")
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

  defp install_missing_deps(root, missing_deps) do
    case missing_deps do
      [] ->
        :ok

      [head | tail] ->
        package = "#{head.name.user}/#{head.name.project}@#{Elm.Version.to_string(head.version)}"

        args = ["install", "--yes", package]
        options = [out: :string, err: :string, dir: root, in: "Y"]

        result =
          case Porcelain.exec(@elm_json_binary, args, options) do
            %Porcelain.Result{status: 0} ->
              install_missing_deps(root, tail)

            %Porcelain.Result{err: err, status: 1} ->
              Logger.error("elm install error: #{err} \n")

              :error
          end
    end
  end

  defp elm_init(root) do
    binary = Path.join(base_path(), "elm")
    args = ["init"]
    options = [out: :string, err: :string, dir: root, in: "Y"]
    result = Porcelain.exec(binary, args, options)
    Logger.info("elm init\nexit: #{inspect(result)}\n")

    case result do
      %Porcelain.Result{status: 0} ->
        case File.cp(Path.join(root, "elm.json"), Path.join(root, "elm.json.original")) do
          :ok ->
            :ok

          {:error, reason} ->
            Sentry.capture_message("elm init filesystem error",
              extra: %{
                reason: reason,
                elm_version: "0.19.1"
              }
            )

            {:error, reason}
        end

      %Porcelain.Result{status: other} ->
        Sentry.capture_message("elm init process error",
          extra: %{
            stderr: result.err,
            stdout: result.out,
            status: result.status,
            elm_version: "0.19.1"
          }
        )

        {:error, "init exited with code #{other}"}

      {:error, reason} ->
        Sentry.capture_message("elm init startup error",
          extra: %{reason: reason, elm_version: "0.19.1"}
        )

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
      "elm-version" => "0.19.1",
      "source-directories" => ["src"],
      "dependencies" => %{
        "direct" => %{},
        "indirect" => %{}
      },
      "test-dependencies" => %{
        "direct" => %{},
        "indirect" => %{}
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
                  problems:
                    Enum.map(Map.fetch!(a, "problems"), fn b ->
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
            problem = %Elm.Error.GeneralProblem{
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

  defp parse_chunk(%{
         "bold" => bold,
         "underline" => underline,
         "color" => color,
         "string" => string
       }) do
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

  defp base_path, do: Path.join(:code.priv_dir(:ellie), "bin/0.19.1")
end

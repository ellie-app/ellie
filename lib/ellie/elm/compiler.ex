defmodule Ellie.Elm.Compiler do
  alias Ellie.Elm.Project
  alias Ellie.Elm.Name

  @bin_path Path.expand("../../../priv/bin", __DIR__)

  def init(root) do
    write_project!(root, %Project{}) # write default project file
    File.mkdir_p!(Path.join(root, "src"))
    with :ok <- install_by_name(root, Name.core()),
      :ok <- install_by_name(root, Name.browser()),
      :ok <- install_by_name(root, Name.html())
    do
      :ok
    else
      error -> error
    end
  end

  defp install_by_name(root, name) do
    binary = Path.join(@bin_path, "elm")
    args = ["install", Name.to_string(name)]
    options = [out: nil, err: nil, dir: root]
    result = Porcelain.exec(binary, args, options)
    case result do
      %Porcelain.Result{status: 0} ->
        :ok
      %Porcelain.Result{status: other} ->
        {:error, "install exited with code #{other}"}
    end
  end

  require IEx

  def compile(options) do
    %{root: root, entry: entry, output: output} = Enum.into(options, %{})
    binary = Path.join(@bin_path, "elm")
    args = ["make", entry, "--output", output, "--report", "json"]
    options = [dir: root, out: :string, err: :string]
    result = Porcelain.exec(binary, args, options)
    case result do
      %Porcelain.Result{err: "", status: 0} ->
        {:ok, nil}
      %Porcelain.Result{err: string, status: 0} ->
        case Poison.decode(string) do
          {:ok, error} -> {:ok, error}
          _ -> {:error, "unparseable compiler result"}
        end
      %Porcelain.Result{err: err, out: out, status: other} ->
        {:error, "compiler exited with code #{other}\nstdout: #{out}\nstderr:#{err}"}
    end
  end

  def format(code) do
    binary = Path.join(@bin_path, "elm-format")
    args = ["'--stdin'"]
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
    project = read_project!(root)
    if not MapSet.equal?(packages, project.deps) do
      updated = %{ project | deps: packages, trans_deps: MapSet.new() }
      write_project!(root, updated)
    end
    :ok
  end

  def dependencies(root) do
    project = read_project!(root)
    {:ok, project.deps}
  end

  defp read_project!(root) do
    project_path = Path.expand("./elm.json", root)
    data = File.read!(project_path)
    project_json = Poison.decode!(data)
    {:ok, project} = Project.from_json(project_json)
    project
  end

  defp write_project!(root, project) do
    project_path = Path.expand("./elm.json", root)
    project_json = Project.to_json(project)
    data = Poison.encode!(project_json)
    File.write!(project_path, data)
  end
end

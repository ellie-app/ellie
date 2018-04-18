defmodule Ellie.Workspace do
  alias Ellie.Elm.Platform
  use Agent
  defstruct [:location, :packages, :elm_hash, :html_hash, :error]

  def open(user, version) do
    current = get(user, version)
    case current do
      nil -> open_help(user, version)
      workspace -> {:ok, workspace}
    end
  end

  defp open_help(user, version) do
    location = Path.expand("../../.local_tmp/#{user.id}", __DIR__)
    File.rm_rf!(location)
    File.mkdir_p!(location)
    with :ok <- Platform.setup!(version, location),
      {:ok, deps} <- Platform.dependencies!(version, location)
    do
      workspace = %Ellie.Workspace{
        location: location,
        packages: deps,
        elm_hash: "",
        html_hash: "",
        error: nil
      }
      put(user, version, workspace)
      {:ok, workspace}
    else
      _ -> {:error, :install_failure}
    end
  end

  def destroy(user, version) do
    current = get(user, version)
    case current do
      nil ->
        :ok
      workspace ->
        File.rm_rf!(workspace.location)
        remove(user, version)
        :ok
    end
  end

  def result(user, version) do
    case get(user, version) do
      nil ->
        {:error, "workspace not compiled"}
      current ->
        {:ok, Path.join(current.location, "build.js")}
    end
  end

  def compile(user, version, elm_code, packages) do
    {:ok, workspace} = open(user, version)
    new_elm_hash = Murmur.hash_x64_128(elm_code)
    elm_changed = new_elm_hash != workspace.elm_hash
    packages_changed = not MapSet.equal?(packages, workspace.packages)
    needs_compile = packages_changed or elm_changed
    elm_path = Path.join(workspace.location, "src/Main.elm")

    if packages_changed do
      Platform.install!(version, workspace.location, packages)
    end

    if elm_changed do
      File.write!(elm_path, elm_code)
    end

    new_error =
      if needs_compile do
        {:ok, error} = Platform.compile!(version, root: workspace.location, entry: "src/Main.elm", output: "build.js")
        error
      else
        workspace.error
      end

    updated_workspace = %{workspace | elm_hash: new_elm_hash, error: new_error, packages: packages}
    put(user, version, updated_workspace)
    {:ok, new_error}
  end


  def start_link() do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  defp get(user, version) do
    Agent.get __MODULE__, fn state ->
      state
      |> Map.get(user.id, %{})
      |> Map.get(version)
    end
  end

  defp put(user, version, workspace) do
    Agent.update __MODULE__, fn state ->
      Map.update(state, user.id, %{}, &Map.put(&1, version, workspace))
    end
  end

  defp remove(user, version) do
    Agent.update __MODULE__, fn state ->
      Map.update(state, user.id, %{}, &Map.delete(&1, version))
    end
  end
end

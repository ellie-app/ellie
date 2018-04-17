defmodule Ellie.Workspace do
  alias Ellie.Elm.Compiler
  defstruct [:location, :packages, :elm_hash, :html_hash, :error]

  def open(user) do
    current = get(user)
    case current do
      nil ->
        location = Path.expand("../../.local_tmp/#{user.id}", __DIR__)
        File.rm_rf!(location)
        File.mkdir_p!(location)
        with :ok <- Compiler.init(location),
          {:ok, deps} <- Compiler.dependencies(location)
        do
          workspace = %Ellie.Workspace{
            location: location,
            packages: deps,
            elm_hash: "",
            html_hash: "",
            error: nil
          }
          put(user, workspace)
          {:ok, workspace}
        else
          _ -> {:error, :install_failure}
        end
      workspace -> {:ok, workspace}
    end
  end


  def destroy(user) do
    current = get(user)
    case current do
      nil ->
        :ok
      workspace ->
        File.rm_rf!(workspace.location)
        remove(user)
        :ok
    end
  end

  def result(user) do
    case get(user) do
      nil ->
        {:error, "workspace not compiled"}
      current ->
        {:ok, Path.join(current.location, "build.js")}
    end
  end

  def compile(user, elm_code, packages) do
    {:ok, workspace} = open(user)
    new_elm_hash = Murmur.hash_x64_128(elm_code)
    elm_changed = new_elm_hash != workspace.elm_hash
    packages_changed = not MapSet.equal?(packages, workspace.packages)
    needs_compile = packages_changed or elm_changed
    elm_path = Path.join(workspace.location, "src/Main.elm")

    if packages_changed do
      Compiler.install(workspace.location, packages)
    end

    if elm_changed do
      File.write!(elm_path, elm_code)
    end

    new_error =
      if needs_compile do
        {:ok, error} = Compiler.compile(root: workspace.location, entry: "src/Main.elm", output: "build.js")
        error
      else
        workspace.error
      end

    updated_workspace = %{workspace | elm_hash: new_elm_hash, error: new_error, packages: packages}
    put(user, updated_workspace)
    {:ok, new_error}
  end

  defp get(user) do
    GenServer.call(Ellie.Workspace.Entry.server, {:get, user.id})
  end

  defp put(user, workspace) do
    GenServer.cast(Ellie.Workspace.Entry.server, {:put, user.id, workspace})
  end

  defp remove(user) do
    GenServer.cast(Ellie.Workspace.Entry.server, {:remove, user.id})
  end
end

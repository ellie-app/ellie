defmodule Ellie.Workspace do
  alias Ellie.Elm.Platform
  use GenServer
  defstruct [:location, :packages, :elm_hash, :html_hash, :error]

  def open(user, version) do
    current = get(user)
    case current do
      nil -> open_help(user, version)
      workspace -> {:ok, workspace}
    end
  end

  defp open_help(user, version) do
    location = Path.expand("../../.local_tmp/workspaces/#{user.id}", __DIR__)
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
      put(user, workspace)
      {:ok, workspace}
    else
      _ -> {:error, :install_failure}
    end
  end

  def result(user, version) do
    case get(user) do
      nil ->
        {:error, "workspace not compiled"}
      current ->
        {:ok, Platform.result!(version, current.location)}
    end
  end

  def compile(user, version, elm_code, packages) do
    {:ok, workspace} = open(user, version)
    new_elm_hash = Murmur.hash_x64_128(elm_code)
    elm_changed = new_elm_hash != workspace.elm_hash
    packages_changed = not MapSet.equal?(packages, workspace.packages)
    needs_compile = packages_changed or elm_changed

    if packages_changed do
      Platform.install!(version, workspace.location, packages)
    end

    new_error =
      if needs_compile do
        {:ok, error} = Platform.compile!(version, workspace.location, elm_code)
        error
      else
        workspace.error
      end

    updated_workspace = %{workspace | elm_hash: new_elm_hash, error: new_error, packages: packages}
    put(user, updated_workspace)
    {:ok, new_error}
  end

  def cleanup_after(user, pid) do
    GenServer.cast(__MODULE__, {:cleanup_after, user.id, pid})
  end

  defp get(user) do
    GenServer.call(__MODULE__, {:get, user.id})
  end

  defp put(user, workspace) do
    GenServer.cast(__MODULE__, {:put, user.id, workspace})
  end

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    {:ok, %{workspaces: %{}, monitors: %{}}}
  end

  def handle_call({:get, user_id}, _from, state) do
    {:reply, Map.get(state.workspaces, user_id), state}
  end

  def handle_cast({:put, user_id, workspace}, state) do
    data = %{ state | workspaces: Map.put(state.workspaces, user_id, workspace) }
    {:noreply, data}
  end

  def handle_cast({:cleanup_after, user_id, pid}, state) do
    Process.monitor(pid)
    data = %{ state | monitors: Map.put(state.monitors, pid, user_id) }
    {:noreply, data}
  end

  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    with {:ok, user_id} <- Map.fetch(state.monitors, pid),
      {:ok, workspace} <- Map.fetch(state.workspaces, user_id)
    do
      File.rm_rf!(workspace.location)
      state = %{workspaces: Map.delete(state.workspaces, user_id), monitors: Map.delete(state.monitors, pid)}
      {:noreply, state}
    else
      _ -> {:noreply, state}
    end
  end

  def handle_info(message, state) do
    super(message, state)
  end
end

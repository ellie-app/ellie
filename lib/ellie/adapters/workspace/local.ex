defmodule Ellie.Adapters.Workspace.Local do
  alias Elm.Platform
  alias Elm.Version
  alias Elm.Package
  alias Elm.Project
  alias Elm.Error
  alias Data.Uuid
  use GenServer


  defstruct [:location, :packages, :elm_hash, :error, :version]

  # API

  @behaviour Ellie.Domain.Workspace

  @spec create() :: {:ok, Uuid.t} | :error
  def create() do
    {:ok, Uuid.new()}
  end

  @spec watch(id :: Uuid.t, process :: pid) :: :unit
  def watch(id, process) do
    GenServer.cast(__MODULE__, {:cleanup_after, id, process})
    :unit
  end

  @spec result(id :: Uuid.t) :: {:ok, {Path.t, String.t}} | :error
  def result(id) do
    case get(id) do
      nil ->
        :error
      current ->
        path = Path.join(current.location, "build.js")
        if is_nil(current.error) && File.exists?(path) do
          {:ok, {path, current.elm_hash}}
        else
          :error
        end
    end
  end

  @spec dependencies(id :: Uuid.t, version :: Version.t) :: {:ok, MapSet.t(Package.t)} | :error
  def dependencies(id, version) do
    case open(id, version) do
      :ok ->
        {:ok, get(id).packages}
      :error ->
        :error
    end
  end

  @spec compile(id :: Uuid.t, version :: Version.t, elm_code :: String.t, packages :: MapSet.t(Package.t)) :: {:ok, Error.t | nil} | :error
  def compile(id, version, elm_code, packages) do
    case open(id, version) do
      :ok ->
        workspace = get(id)
        new_elm_hash = Murmur.hash_x64_128(elm_code)
        elm_changed = new_elm_hash != workspace.elm_hash
        packages_changed = not MapSet.equal?(packages, workspace.packages)
        needs_compile = packages_changed or elm_changed

        compile_result =
          if needs_compile do
            Platform.compile(workspace.location, [
              source: elm_code,
              output: "build.js",
              project: %Project{elm_version: version, dependencies: packages}
            ])
          else
            {:ok, workspace.error}
          end

        case compile_result do
          {:ok, error} ->
            updated_workspace = %{workspace | elm_hash: new_elm_hash, error: error, packages: packages}
            put(id, updated_workspace)
            {:ok, error}
          :error ->
            :error
        end
      :error ->
        :error
    end
  end

  # HELPERS

  defp location_for_id(id) do
    Path.expand("../../../../.local_tmp/workspaces/#{Uuid.to_string(id)}", __DIR__)
  end

  defp open(id, version) do
    current = get(id)
    case current do
      nil -> open_help(id, version)
      workspace ->
        if workspace.version == version && File.exists?(workspace.location) do
          :ok
        else
          open_help(id, version)
        end
    end
  end

  defp open_help(id, version) do
    location = location_for_id(id)
    File.rm_rf!(location)
    File.mkdir_p!(location)
    case Platform.setup(location, version) do
      {:ok, project} ->
        workspace = %__MODULE__{
          version: version,
          location: location,
          packages: project.dependencies,
          elm_hash: "",
          error: nil
        }
        put(id, workspace)
        :ok
      _ ->
        :error
    end
  end

  defp get(id) do
    GenServer.call(__MODULE__, {:get, id})
  end

  defp put(id, workspace) do
    GenServer.cast(__MODULE__, {:put, id, workspace})
  end

  # SERVER

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
    {:noreply, %{state | workspaces: Map.put(state.workspaces, user_id, workspace)}}
  end

  def handle_cast({:cleanup_after, user_id, pid}, state) do
    Process.monitor(pid)
    {:noreply, %{state | monitors: Map.put(state.monitors, pid, user_id)}}
  end

  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    with {:ok, id} <- Map.fetch(state.monitors, pid),
      {:ok, workspace} <- Map.fetch(state.workspaces, id)
    do
      File.rm_rf!(workspace.location)
      state = %{workspaces: Map.delete(state.workspaces, id), monitors: Map.delete(state.monitors, pid)}
      {:noreply, state}
    else
      _ -> {:noreply, state}
    end
  end

  def handle_info(message, state) do
    super(message, state)
  end
end

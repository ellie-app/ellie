defmodule Ellie.Workspace.Entry do
  def server do
    Process.whereis(__MODULE__) ||
      raise "could not find process Ellie.Workspace.Entry. Have you started the supervisor?"
  end

  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, [name: __MODULE__])
  end

  def get(server, user) do
    GenServer.call(server, {:get, user.id})
  end

  def put(server, user, workspace) do
    GenServer.cast(server, {:put, user.id, workspace})
  end

  ## Callbacks

  def init(:ok) do
    {:ok, %{}}
  end

  def handle_call({:get, user_id}, _from, state) do
    {:reply, Map.get(state, user_id), state}
  end

  def handle_cast({:put, user_id, workspace}, state) do
    {:noreply, Map.put(state, user_id, workspace)}
  end

  def handle_cast({:remove, user_id}, state) do
    {:noreply, Map.delete(state, user_id)}
  end
end

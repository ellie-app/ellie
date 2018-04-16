defmodule Ellie.Search.Server do
  alias Ellie.Search.Helpers

  def server do
    Process.whereis(__MODULE__) ||
      raise "could not find process Ellie.Search.Server. Have you started the supervisor?"
  end

  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, [name: __MODULE__])
  end

  def search(server, query) do
    GenServer.call(server, {:search, query})
  end

  ## Callbacks

  def init(:ok) do
    Task.start fn -> Helpers.reload() end
    timer = Process.send_after(self(), :reload, 1_000 * 60 * 15)
    {:ok, %{timer: timer}}
  end

  def handle_call({:search, query}, _from, state) do
    {:reply, Helpers.search(query, 0.5), state}
  end

  def handle_info(:reload, _state) do
    Task.start fn -> Helpers.reload() end
    timer = Process.send_after(self(), :reload, 1_000 * 60 * 15)
    {:ok, %{timer: timer}}
  end

  def handle_info(msg, state) do
    super(msg, state)
  end
end

defmodule Ellie.Embed do
  alias Ellie.Elm.Platform
  alias Ellie.Revision
  use Agent

  @base_path Path.expand("../../.local_tmp/embeds", __DIR__)

  def result(%Revision{} = revision) do
    case get_entry(revision) do
      {:finished, nil} ->
        root = Path.join(@base_path, "#{revision.project_id}/#{revision.revision_number}")
        {:ok, Platform.result!(revision.elm_version, root)}
      _ -> :error
    end
  end

  def compile(%Revision{} = revision) do
    case get_entry(revision) do
      :working ->
        :working

      {:finished, error} ->
        {:finished, error}

      _ ->
        put_entry(revision, :working)
        task = fn -> Task.async fn -> do_compile(revision) end end
        {:started, task}
    end
  end

  defp do_compile(revision) do
    root = Path.join(@base_path, "#{revision.project_id}/#{revision.revision_number}")
    File.rm_rf!(root)
    File.mkdir_p!(Path.join(root, "src"))
    with :ok <- Platform.setup!(revision.elm_version, root),
      :ok <- Platform.install!(revision.elm_version, root, MapSet.new(revision.packages)),
      {:ok, error} <- Platform.compile!(revision.elm_version, root, revision.elm_code)
    do
      put_entry(revision, {:finished, error})
      {:ok, error}
    else
      error ->
        put_entry(revision, :failed)
        error
    end
  end

  def start_link() do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  defp get_entry(revision) do
    Agent.get __MODULE__, fn state -> Map.get(state, {revision.project_id, revision.revision_number}) end
  end

  defp put_entry(revision, status) do
    Agent.update __MODULE__, fn state -> Map.put(state, {revision.project_id, revision.revision_number}, status) end
  end
end

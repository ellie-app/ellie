defmodule Ellie.Embed do
  alias Ellie.Elm.Platform
  alias Ellie.Revision
  use Agent

  @base_path Path.expand("../../.local_tmp/embeds", __DIR__)

  def result(project_id, revision_number) do
    case get_entry({project_id, revision_number}) do
      {:finished, nil} ->
        {:ok, Path.join(@base_path, "#{project_id}/#{revision_number}/build.js")}
      _ -> :error
    end
  end

  def compile(%Revision{} = revision) do
    rid = {revision.project_id, revision.revision_number}
    case get_entry(rid) do
      :working ->
        :working

      {:finished, error} ->
        {:finished, error}

      _ ->
        put_entry(rid, :working)
        task = fn -> Task.async fn -> do_compile(rid, revision) end end
        {:started, task}
    end
  end

  defp do_compile(rid, revision) do
    root = Path.join(@base_path, "#{revision.project_id}/#{revision.revision_number}")
    File.rm_rf!(root)
    File.mkdir_p!(Path.join(root, "src"))
    File.write!(Path.join(root, "src/Main.elm"), revision.elm_code)
    with :ok <- Platform.setup!(revision.elm_version, root),
      :ok <- Platform.install!(revision.elm_version, root, MapSet.new(revision.packages)),
      {:ok, error} <- Platform.compile!(revision.elm_version, root: root, entry: "src/Main.elm", output: "build.js")
    do
      put_entry(rid, {:finished, error})
      {:ok, error}
    else
      error ->
        put_entry(rid, :failed)
        error
    end
  end

  def start_link() do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  defp get_entry(rid) do
    Agent.get __MODULE__, fn state -> Map.get(state, rid) end
  end

  defp put_entry(rid, status) do
    Agent.update __MODULE__, fn state -> Map.put(state, rid, status) end
  end

  defp remove_entry(rid) do
    Agent.update __MODULE__, fn state -> Map.delete(state, rid) end
  end
end

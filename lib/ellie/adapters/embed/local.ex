defmodule Ellie.Adapters.Embed.Local do
  alias Elm.Platform
  alias Elm.Project
  alias Ellie.Types.Revision
  use Agent

  @base_path Path.expand("../../.local_tmp/embeds", __DIR__)

  def result(%Revision{} = revision) do
    case get_entry(revision) do
      {:finished, nil} ->
        output = Path.join([@base_path, revision.project_id, revision.revision_number, "build.js"])
        if File.exists?(output) do
          {:ok, output}
        else
          :error
        end
      _ ->
        :error
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
    File.mkdir_p!(root)

    result =
      Platform.compile(root, [
        source: revision.elm_code,
        output: "embed.js",
        project: %Project{
          dependencies: revision.packages,
          elm_version: revision.elm_version
        }
      ])

    case result do
      {:ok, error} ->
        put_entry(revision, {:finished, error})
        {:ok, error}
      :error ->
        put_entry(revision, :failed)
        :error
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

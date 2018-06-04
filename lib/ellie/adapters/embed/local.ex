defmodule Ellie.Adapters.Embed.Local do
  alias Elm.Platform
  alias Elm.Project
  alias Ellie.Types.PrettyId
  alias Ellie.Types.Revision
  use Agent

  @base_path Path.expand("../../../../.local_tmp/embeds", __DIR__)

  def result(%Revision{} = revision) do
    case get_entry(revision) do
      {:finished, nil, _last_access} ->
        output = Path.join([@base_path, to_string(revision.id), "embed.js"])
        if File.exists?(output) do
          put_entry(revision, {:finished, nil, :os.system_time(:second)})
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

      {:finished, error, _last_access} ->
        put_entry(revision, {:finished, nil, :os.system_time(:second)})
        {:finished, error}

      _ ->
        put_entry(revision, :working)
        task = fn -> Task.async fn -> do_compile(revision) end end
        {:started, task}
    end
  end

  def cleanup(minutes_old) do
    if File.exists?(@base_path) do
      @base_path
      |> File.ls!()
      |> Enum.each(fn id_string ->
          root = Path.join(@base_path, id_string)
          with {:ok, id} <- PrettyId.cast(id_string),
                entry <- get_entry_by_id(id)
          do
            case entry do
              nil ->
                File.rm_rf!(root)
                :ok
              {:finished, _error, last_accessed} ->
                if :os.system_time(:second) - last_accessed >= minutes_old * 60 do
                  File.rm_rf!(root)
                  delete_entry_by_id(id)
                end
                :ok
              _ ->
                :ok
            end
          else
            _ ->
              :ok
          end
        end)
        :unit
    else
      :unit
    end
  end

  defp do_compile(revision) do
    root = Path.join(@base_path, to_string(revision.id))
    File.rm_rf!(root)
    File.mkdir_p!(root)

    with {:ok, _p} <- Platform.setup(root, revision.elm_version),
         project <- %Project{dependencies: revision.packages, elm_version: revision.elm_version},
         {:ok, error} <- Platform.compile(root, [source: revision.elm_code, output: "embed.js", project: project])
    do
      put_entry(revision, {:finished, error, :os.system_time(:second)})
      {:ok, error}
    else
      _ ->
        put_entry(revision, :failed)
        :error
    end
  end

  def start_link() do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  defp get_entry(revision) do
    Agent.get __MODULE__, fn state -> Map.get(state, revision.id) end
  end

  defp get_entry_by_id(id) do
    Agent.get __MODULE__, fn state -> Map.get(state, id) end
  end

  defp put_entry(revision, status) do
    Agent.update __MODULE__, fn state -> Map.put(state, revision.id, status) end
  end

  defp delete_entry_by_id(id) do
    Agent.update __MODULE__, fn state -> Map.delete(state, id) end
  end
end

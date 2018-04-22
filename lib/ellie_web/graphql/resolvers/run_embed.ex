defmodule EllieWeb.Graphql.Resolvers.RunEmbed do
  alias Ellie.Embed
  alias Ellie.Repo
  alias Ellie.Revision

  def call(%{project_id: project_id, revision_number: revision_number}, _stuff) do
    case Repo.get_by(Revision, project_id: project_id, revision_number: revision_number) do
      nil ->
        {:error, "no revision"}
      revision ->
        case Embed.compile(revision) do
          {:finished, error} ->
            {:ok, %{error: error}}
          :working ->
            {:ok, nil}
          {:started, task_fn} ->
            Task.start fn ->
              topic = "#{project_id}/#{revision_number}"
              case Task.await(task_fn.(), :infinity) do
                {:ok, error} -> Absinthe.Subscription.publish(EllieWeb.Endpoint, %{error: error}, embed: topic)
                {:error, message} -> Absinthe.Subscription.publish(EllieWeb.Endpoint, %{message: message}, embed: topic)
              end
            end
            {:ok, nil}
        end
    end
  end

  def call(_args, _context) do
    {:ok, false}
  end

end

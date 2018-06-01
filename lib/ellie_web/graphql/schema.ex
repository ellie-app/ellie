defmodule EllieWeb.Graphql.Schema do
  alias Absinthe.Subscription
  alias Absinthe.Resolution.Helpers
  alias Data.Uuid
  alias Ellie.Domain.Api
  alias Ellie.Domain.Search
  alias Ellie.Domain.Workspace
  alias Ellie.Domain.Embed
  alias Ellie.Types.PrettyId
  alias EllieWeb.Token
  alias Elm.Platform

  use Absinthe.Schema

  import_types EllieWeb.Graphql.Types.Unit
  import_types EllieWeb.Graphql.Types.Uuid
  import_types EllieWeb.Graphql.Types.Elm.Version
  import_types EllieWeb.Graphql.Types.Elm.Name
  import_types EllieWeb.Graphql.Types.Elm.Package
  import_types EllieWeb.Graphql.Types.Elm.Error
  import_types EllieWeb.Graphql.Types.Elm.Docs
  import_types EllieWeb.Graphql.Types.PrettyId
  import_types EllieWeb.Graphql.Types.Revision
  import_types EllieWeb.Graphql.Types.WorkspaceUpdate
  import_types EllieWeb.Graphql.Types.EmbedUpdate

  def plugins do
    Absinthe.Plugin.defaults() ++ [EllieWeb.Graphql.Plugins.Sentry]
  end

  query do
    field :revision, non_null(:revision) do
      arg :id, non_null(:project_id)
      resolve fn %{id: id}, _ctx ->
        {:ok, Api.retrieve_revision(id)}
      end
    end

    field :package_search, non_null(list_of(non_null(:elm_package))) do
      arg :query, non_null(:string)
      resolve fn %{query: query}, _ctx ->
        {:ok, Search.search(query)}
      end
    end

    field :packages, non_null(list_of(non_null(:elm_package))) do
      arg :packages, non_null(list_of(non_null(:elm_package_input)))
      resolve fn %{packages: packages}, _ctx ->
        {:ok, Enum.map(packages, &EllieWeb.Graphql.Types.Elm.Package.from_input/1)}
      end
    end
  end

  mutation do
    field :authenticate, non_null(:string) do
      resolve fn _args, _ctx ->
        case Workspace.create() do
          {:ok, workspace} -> {:ok, Token.sign(Uuid.to_string(workspace))}
          :error -> {:error, "Couldn't create workspace"}
        end
      end
    end

    field :create_revision, non_null(:revision) do
      middleware EllieWeb.Graphql.Middleware.RequireWorkspace
      arg :inputs, non_null(:revision_input)
      resolve fn %{inputs: revision}, _ctx ->
        inputs =
          revision
          |> Map.to_list()
          |> Keyword.update!(:packages, fn ps -> Enum.map(ps, &EllieWeb.Graphql.Types.Elm.Package.from_input/1) end)
        case Api.create_revision(inputs) do
          {:ok, revision} -> {:ok, revision}
          :error -> {:error, "failed to create revision"}
        end
      end
    end

    field :attach_to_workspace, non_null(:unit) do
      middleware EllieWeb.Graphql.Middleware.RequireWorkspace
      arg :elm_version, non_null(:elm_version)
      resolve fn
        %{elm_version: version}, %{context: %{workspace: workspace}} ->
          Task.start(fn ->
            case Workspace.dependencies(workspace, version) do
              {:ok, packages} ->
                data = %{packages: MapSet.to_list(packages)}
                Subscription.publish(EllieWeb.Endpoint, data, workspace: workspace)
              :error ->
                data = %{message: "Failed to attach to workspace"}
                Subscription.publish(EllieWeb.Endpoint, data, workspace: workspace)
            end
          end)
          {:ok, :unit}
        _args, _ctx ->
          {:error, "Attach to workspace must run on socket connection"}
      end
    end

    field :format_code, non_null(:string) do
      middleware EllieWeb.Graphql.Middleware.RequireWorkspace
      arg :elm_version, non_null(:elm_version)
      arg :code, non_null(:string)
      resolve fn %{elm_version: version, code: code}, _ctx ->
        Helpers.async(fn ->
          case Platform.format(code, version) do
            {:ok, code} -> {:ok, code}
            :error -> {:error, "Failed to format code"}
          end
        end)
      end
    end

    field :compile, non_null(:unit) do
      middleware EllieWeb.Graphql.Middleware.RequireWorkspace
      arg :elm_version, non_null(:elm_version)
      arg :elm_code, non_null(:string)
      arg :packages, non_null(list_of(non_null(:elm_package_input)))
      resolve fn
        %{elm_version: elm_version, elm_code: elm_code, packages: packages}, %{context: %{workspace: workspace}} ->
          real_packages =
            packages
            |> Enum.map(&EllieWeb.Graphql.Types.Elm.Package.from_input/1)
            |> MapSet.new()
          Task.start(fn ->
            case Workspace.compile(workspace, elm_version, elm_code, real_packages) do
              {:ok, error} ->
                data = %{error: error}
                Subscription.publish(EllieWeb.Endpoint, data, workspace: workspace)
              :error ->
                data = %{message: "Could not compile"}
                Subscription.publish(EllieWeb.Endpoint, data, workspace: workspace)
            end
          end)
          {:ok, :unit}
        _args, _ctx ->
          {:error, "Compile must run on socket connection"}
      end
    end

    field :run_embed, :embed_ready do
      arg :id, non_null(:project_id)
      resolve fn %{id: id}, _ctx ->
        case Api.retrieve_revision(id) do
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
                  case Task.await(task_fn.(), :infinity) do
                    {:ok, error} -> Subscription.publish(EllieWeb.Endpoint, %{error: error}, embed: to_string(id))
                    :error -> Subscription.publish(EllieWeb.Endpoint, %{message: "Could not compile"}, embed: to_string(id))
                  end
                end
                {:ok, nil}
            end
          end
      end
    end
  end

  subscription do
    field :workspace, non_null(:workspace_update) do
      config fn
        _args, %{context: %{workspace: workspace}} -> {:ok, topic: to_string(workspace)}
        _args, _ -> {:error, "Bad workspace state"}
      end
    end

    field :embed, non_null(:embed_update) do
      arg :id, non_null(:project_id)
      config fn
        %{id: id}, _stuff ->
          {:ok, topic: to_string(id)}
        _args, _stuff ->
          {:error, "Insufficient info"}
      end
    end
  end
end

defmodule EllieWeb.Graphql.Schema do
  alias Absinthe.Subscription
  alias Absinthe.Resolution.Helpers
  alias Ellie.Domain.Api
  alias Ellie.Domain.Search
  alias Ellie.Domain.Workspace
  alias Ellie.Domain.Embed
  alias EllieWeb.Auth
  alias Elm.Platform
  alias Elm.Package

  use Absinthe.Schema
  import_types EllieWeb.Graphql.Types.Unit
  import_types EllieWeb.Graphql.Types.Uuid
  import_types EllieWeb.Graphql.Types.Version
  import_types EllieWeb.Graphql.Types.Name
  import_types EllieWeb.Graphql.Types.ProjectId
  import_types EllieWeb.Graphql.Types.Elm.Error
  import_types EllieWeb.Graphql.Types.Elm.Docs
  import_types EllieWeb.Graphql.Types

  def middleware(middleware, %{identifier: identifier} = field, object) do
    middleware_spec = {{__MODULE__, :get_string_key}, identifier}
    Absinthe.Schema.replace_default(middleware, middleware_spec, field, object)
  end
  def middleware(middleware, _field, _object) do
    middleware
  end
  def get_string_key(%{source: source} = res, key) do
    %{res | state: :resolved, value: Map.get(source, key, Map.get(source, Atom.to_string(key))) }
  end

  def plugins() do
    [Absinthe.Middleware.Dataloader] ++ Absinthe.Plugin.defaults()
  end

  query do
    field :user, non_null(:user) do
      middleware EllieWeb.Graphql.Middleware.Auth
      resolve fn _args, %{context: %{current_user: current_user}} ->
        {:ok, current_user}
      end
    end

    field :revision, non_null(:revision) do
      arg :project_id, non_null(:project_id)
      arg :revision_number, non_null(:integer)
      resolve fn %{project_id: project_id, revision_number: revision_number}, _ctx ->
        {:ok, Api.retrieve_revision(project_id, revision_number)}
      end
    end

    field :package_search, non_null(list_of(non_null(:package))) do
      arg :query, non_null(:string)
      resolve fn %{query: query}, _ctx ->
        {:ok, Search.search(query)}
      end
    end

    field :packages, non_null(list_of(non_null(:package))) do
      arg :packages, non_null(list_of(non_null(:package_input)))
      resolve fn %{packages: packages}, _ctx ->
        {:ok, Enum.map(packages, &%Elm.Package{name: &1.name, version: &1.version})}
      end
    end
  end


  mutation do
    field :authenticate, non_null(:user_auth) do
      resolve fn
        _args, %{context: %{current_user: current_user}} ->
          {:ok, %{user: current_user, token: Auth.sign(current_user), terms_version: 1}}
        _args, _ctx ->
          case Api.create_user() do
            {:ok, user} -> {:ok, %{user: user, token: Auth.sign(user), terms_version: 1}}
            :error -> {:error, "Failed to create user"}
          end
      end
    end

    field :create_revision, non_null(:revision) do
      arg :inputs, non_null(:revision_update_input)
      middleware EllieWeb.Graphql.Middleware.Auth
      resolve fn %{inputs: revision}, %{context: %{current_user: user}} ->
        inputs =
          revision
          |> Map.to_list()
          |> Keyword.update!(:packages, fn ps -> Enum.map(ps, &%Package{name: &1.name, version: &1.version}) end)
        case Api.create_revision(user, inputs) do
          {:ok, revision} -> {:ok, revision}
          :error -> {:error, "failed to create revision"}
        end
      end
    end

    field :update_revision, non_null(:revision) do
      arg :inputs, non_null(:revision_update_input)
      arg :project_id, non_null(:project_id)
      middleware EllieWeb.Graphql.Middleware.Auth
      resolve fn %{inputs: inputs, project_id: project_id}, %{context: %{current_user: user}} ->
        revision =
          inputs
          |> Map.to_list()
          |> Keyword.update!(:packages, fn ps -> Enum.map(ps, &%Package{name: &1.name, version: &1.version}) end)
          |> Keyword.put(:project_id, project_id)
        case Api.update_revision(user, revision) do
          {:ok, revision} -> {:ok, revision}
          :error -> {:error, "Failed to update revision"}
        end
      end
    end

    field :accept_terms, non_null(:unit) do
      arg :terms, non_null(:integer)
      middleware EllieWeb.Graphql.Middleware.Auth
      resolve fn %{terms: terms}, %{context: %{current_user: user}} ->
        case Api.accept_terms(user, terms) do
          :ok -> {:ok, :unit}
          :error -> {:error, "Failed to accept terms"}
        end
      end
    end

    field :attach_to_workspace, non_null(:unit) do
      arg :elm_version, non_null(:version)
      middleware EllieWeb.Graphql.Middleware.Auth
      resolve fn %{elm_version: version}, %{context: %{current_user: user}} ->
        Task.start(fn ->
          case Workspace.dependencies(user, version) do
            {:ok, packages} ->
              data = %{packages: MapSet.to_list(packages)}
              Subscription.publish(EllieWeb.Endpoint, data, workspace: user.id)
            :error ->
              data = %{message: "Failed to attach to workspace"}
              Subscription.publish(EllieWeb.Endpoint, data, workspace: user.id)
          end
        end)
        {:ok, :unit}
      end
    end

    field :format_code, non_null(:string) do
      arg :elm_version, non_null(:version)
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
      arg :elm_version, non_null(:version)
      arg :elm_code, non_null(:string)
      arg :packages, non_null(list_of(non_null(:package_input)))
      middleware EllieWeb.Graphql.Middleware.Auth
      resolve fn %{elm_version: elm_version, elm_code: elm_code, packages: packages}, %{context: %{current_user: user}} ->
        real_packages =
          packages
          |> Enum.map(&%Package{name: &1.name, version: &1.version})
          |> MapSet.new()
        Task.start(fn ->
          case Workspace.compile(user, elm_version, elm_code, real_packages) do
            {:ok, error} ->
              data = %{error: error}
              Subscription.publish(EllieWeb.Endpoint, data, workspace: user.id)
            :error ->
              data = %{message: "Could not compile"}
              Subscription.publish(EllieWeb.Endpoint, data, workspace: user.id)
          end
        end)
        {:ok, :unit}
      end
    end

    field :update_settings, non_null(:unit) do
      arg :font_size, :string
      arg :font_family, :string
      arg :theme, :theme
      arg :vim_mode, :boolean
      middleware EllieWeb.Graphql.Middleware.Auth
      resolve fn data, %{context: %{current_user: user}} ->
        settings = Enum.filter(data, fn {_, v} -> not is_nil(v) end)
        case Api.update_settings(user, settings) do
          :ok -> {:ok, :unit}
          :error -> {:error, "Could not update settings"}
        end
      end
    end

    field :run_embed, :embed_ready do
      arg :project_id, non_null(:project_id)
      arg :revision_number, non_null(:integer)
      resolve fn %{project_id: project_id, revision_number: revision_number}, _ctx ->
        case Api.retrieve_revision(project_id, revision_number) do
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
                    {:ok, error} -> Subscription.publish(EllieWeb.Endpoint, %{error: error}, embed: topic)
                    {:error, message} -> Subscription.publish(EllieWeb.Endpoint, %{message: message}, embed: topic)
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
        _args, %{context: %{current_user: user}} -> {:ok, topic: to_string(user.id)}
        _args, _ -> {:error, "unauthorized"}
      end
    end

    field :embed, non_null(:embed_update) do
      arg :project_id, non_null(:project_id)
      arg :revision_number, non_null(:integer)
      config fn
        %{project_id: project_id, revision_number: revision_number}, _stuff ->
          {:ok, topic: "#{project_id}/#{revision_number}"}
        _args, _stuff ->
          {:error, "insufficient info"}
      end
    end
  end
end

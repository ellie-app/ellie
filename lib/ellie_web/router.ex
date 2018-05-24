defmodule EllieWeb.Router do
  use EllieWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :put_secure_browser_headers
    plug :put_layout, false
  end

  pipeline :assets do
    plug :accepts, ["javascript"]
  end

  pipeline :api do
    plug :accepts, ["json"]
    plug EllieWeb.Graphql.Context
  end

  pipeline :oembed do
    plug :accepts, ["json"]
  end

  if Application.get_env(:ellie, :env) == :dev do
    scope "/assets" do
      forward "/", PlugProxy,
        upstream: "http://webpack:8080/",
        connect_timeout: :infinity
    end
  end

  scope "/api" do
    pipe_through :api
    forward "/graphiql", Absinthe.Plug.GraphiQL, schema: EllieWeb.Graphql.Schema
    forward "/", Absinthe.Plug, schema: EllieWeb.Graphql.Schema
  end

  scope "/r" do
    pipe_through :assets
    get "/workspace", EllieWeb.ResultController, :workspace
    get "/embed/:id", EllieWeb.ResultController, :embed
  end

  scope "/oembed" do
    pipe_through :oembed
    get "/", EllieWeb.OembedController, :oembed
  end

  scope "/" do
    pipe_through :browser

    get "/new", EllieWeb.PageController, :new_editor

    get "/a/terms/:version", EllieWeb.PageController, :terms

    get "/embed/:project_id/:revision_number", EllieWeb.PageController, :embed_old
    get "/embed/:id", EllieWeb.PageController, :embed

    get "/:project_id/:revision_number", EllieWeb.PageController, :existing_editor_old
    get "/:id", EllieWeb.PageController, :existing_editor

    get "/*path", EllieWeb.PageController, :new_editor
  end
end

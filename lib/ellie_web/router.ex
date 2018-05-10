defmodule EllieWeb.Router do
  use EllieWeb, :router

  pipeline :browser do
    plug :accepts, ["html", "javascript"]
    plug :put_secure_browser_headers
    plug :put_layout, false
  end

  pipeline :api do
    plug :accepts, ["json"]
    plug EllieWeb.Context
  end

  scope "/assets" do
    if Application.get_env(:ellie, :env) == :prod do
      forward "/", Plug.Static, at: "/", from: :ellie, gzip: true, headers: %{"Service-Worker-Allowed" => "/"}
    else
      forward "/", PlugProxy, upstream: "http://webpack:8080/"
    end
  end

  scope "/api" do
    pipe_through :api
    forward "/graphiql", Absinthe.Plug.GraphiQL, schema: EllieWeb.Graphql.Schema
    forward "/", Absinthe.Plug, schema: EllieWeb.Graphql.Schema
  end

  scope "/" do
    pipe_through :browser
    get "/private/result", EllieWeb.PageController, :result_manager
    get "/output/embed/:project_id/:revision_number", EllieWeb.PageController, :embed_manager
    get "/a/terms/:version", EllieWeb.PageController, :terms
    get "/embed/:project_id/:revision_number", EllieWeb.PageController, :embed
    get "/*path", EllieWeb.PageController, :new_editor
  end
end

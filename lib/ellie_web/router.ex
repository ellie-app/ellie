defmodule EllieWeb.Router do
  use EllieWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
    plug EllieWeb.Context
  end

  scope "/", EllieWeb do
    pipe_through :browser # Use the default browser stack
    get "/", PageController, :index
  end

  scope "/api" do
    pipe_through :api
    forward "/graphiql", Absinthe.Plug.GraphiQL, schema: EllieWeb.Schema
    forward "/", Absinthe.Plug, schema: EllieWeb.Schema
  end
end

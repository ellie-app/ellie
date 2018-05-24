defmodule EllieWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :ellie
  use Absinthe.Phoenix.Endpoint
  use Sentry.Phoenix.Endpoint

  socket "/api/sockets", EllieWeb.Graphql.Socket

  # Code reloading can be explicitly enabled under the
  # :code_reloader configuration of your endpoint.
  if code_reloading? do
    socket "/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
  end

  if Application.get_env(:ellie, :env) == :prod do
    plug Plug.Static,
      at: "/assets",
      from: :ellie,
      gzip: true,
      headers: %{"Service-Worker-Allowed" => "/"}
  end

  plug Plug.Logger

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Poison

  plug Plug.MethodOverride
  plug Plug.Head

  plug :clear_cookies, []


  plug EllieWeb.Router

  @doc """
  Callback invoked for dynamically configuring the endpoint.

  It receives the endpoint configuration and checks if
  configuration should be loaded from the system environment.
  """
  def init(_key, config) do
    if config[:load_from_system_env] do
      port = System.get_env("PORT") || raise "expected the PORT environment variable to be set"
      {:ok, Keyword.put(config, :http, [:inet6, port: port])}
    else
      {:ok, config}
    end
  end

  def clear_cookies(conn, _opts) do
    fetched = fetch_cookies(conn)
    Enum.reduce(fetched.req_cookies, fetched, fn {k, _}, c ->
      delete_resp_cookie(c, k)
    end)
  end
end

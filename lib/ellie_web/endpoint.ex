defmodule EllieWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :ellie
  use Absinthe.Phoenix.Endpoint
  use Sentry.Phoenix.Endpoint

  socket("/api/sockets", EllieWeb.Graphql.Socket)

  # Code reloading can be explicitly enabled under the
  # :code_reloader configuration of your endpoint.
  if code_reloading? do
    socket("/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket)
    plug(Phoenix.LiveReloader)
    plug(Phoenix.CodeReloader)
  end

  plug(Plug.Static,
    at: "/assets",
    from: :ellie,
    gzip: true,
    headers: %{"Service-Worker-Allowed" => "/", "Access-Control-Allow-Origin" => "*"}
  )

  plug(Plug.Logger)

  plug(Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Jason
  )

  plug(Plug.MethodOverride)
  plug(Plug.Head)

  plug(:clear_cookies, [])

  plug(EllieWeb.Router)

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
    conn
    |> fetch_cookies()
    |> delete_cookie_if_present("_ga",
      domain: ".ellie-app.com",
      path: "/",
      http_only: false,
      secure: false
    )
    |> delete_cookie_if_present("_gid",
      domain: ".ellie-app.com",
      path: "/",
      http_only: false,
      secure: false
    )
    |> delete_cookie_if_present("ownedProjects",
      domain: ".ellie-app.com",
      path: "/",
      http_only: true,
      secure: true
    )
  end

  defp delete_cookie_if_present(conn, key, opts) do
    if Map.has_key?(conn.req_cookies, key) do
      delete_resp_cookie(conn, key, opts)
    else
      conn
    end
  end
end

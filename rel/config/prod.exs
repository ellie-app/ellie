use Mix.Config

app = System.get_env("APPLICATION_NAME")
env = System.get_env("ENVIRONMENT_NAME")
region = System.get_env("AWS_REGION")
aws = System.find_executable("aws")

cond do
  is_nil(app) ->
    raise "APPLICATION_NAME is unset!"

  is_nil(env) ->
    raise "ENVIRONMENT_NAME is unset!"

  is_nil(aws) ->
    raise "Unable to find `aws` executable!"

  :else ->
    :ok
end

# Pull runtime secrets from Parameter Store
secret_root = "/#{app}-#{env}/"

cmd_options = [
  "ssm",
  "get-parameters-by-path",
  "--region=#{region}",
  "--path=#{secret_root}",
  "--with-decryption",
  "--query=Parameters[*].{Name:Name,Value:Value}"
]

secrets =
  case System.cmd(aws, cmd_options) do
    {json, 0} ->
      json
      |> Poison.Parser.parse!()
      |> Enum.reduce(Map.new(), fn %{"Name" => k, "Value" => v}, acc ->
        Map.put(acc, String.replace_leading(k, secret_root, ""), v)
      end)

    {output, status} ->
      raise "Unable to get secrets, command exited with status #{status}:\n#{output}"
  end

config :porcelain, :goon_driver_path, "#{:code.priv_dir(:ellie)}/bin/goon"
config :porcelain, driver: Porcelain.Driver.Goon

config :ellie, EllieWeb.Endpoint,
  http: [port: 4000],
  url: [host: "localhost", port: 4000],
  # check_origin: ["ellie-app.com"],
  check_origin: false,
  root: ".",
  secret_key_base: Map.get(secrets, "SECRET_KEY_BASE")

config :ellie, Ellie.Repo, url: Map.get(secrets, "DATABASE_URL")

config :sentry,
  dsn: Map.get(secrets, "SENTRY_DSN"),
  api_key: Map.get(secrets, "SENTRY_API_KEY")

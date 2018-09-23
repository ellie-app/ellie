use Mix.Config

# Application name
app = System.get_env("APPLICATION_NAME")
env = System.get_env("ENVIRONMENT_NAME")

# Locate awscli
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

# Pull runtime secrets from S3
bucket_name = "#{app}-#{env}-secrets"
key_name = "secrets.json"

%{"db_url" => db_url,
  "secret_key_base" => secret_key_base,
  "sentry_dsn" => sentry_dsn,
  "sentry_api_key" => sentry_api_key} =
  case System.cmd(aws, ["s3", "cp", "s3://#{bucket_name}/#{key_name}", "-"]) do
    {json, 0} ->
      Poison.Parser.parse!(json)
    {output, status} ->
      raise "Unable to get secrets, command exited with status #{status}:\n#{output}"
  end

config :porcelain, :goon_driver_path, "#{:code.priv_dir(:ellie)}/bin/goon"
config :porcelain, driver: Porcelain.Driver.Goon

config :ellie, EllieWeb.Endpoint,
  http: [port: 4000],
  url: [host: "localhost", port: 4000],
  # TODO: use ["ellie-app.com"]
  check_origin: false,
  root: ".",
  secret_key_base: secret_key_base

config :ellie, Ellie.Repo,
  url: db_url

config :sentry,
  dsn: sentry_dsn,
  api_key: sentry_api_key

#! /usr/bin/env bash

set -e

mkdir -p /app/priv/bin/
mkdir -p /app/priv/elm_home
cp -r /tmp/ellie-bin/* /app/priv/bin/

mix deps.get
mix compile
mix do loadpaths, absinthe.schema.json ./priv/graphql/schema.json

until PGPASSWORD=postgres psql -h "database" -U "postgres" -c '\q'; do
  >&2 echo "Postgres is unavailable - sleeping"
  sleep 1
done

mix ecto.create
mix ecto.migrate
mix phx.server

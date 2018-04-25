#! /usr/bin/env bash

set -e

cd assets
yarn install

until [ -e ../priv/graphql/schema.json ]; do
  >&2 echo "Waiting for graphql schema to be available"
  sleep 1
done

yarn graphqelm --introspection-file ../priv/graphql/schema.json --base Ellie.Api --output elm-stuff/generated/dillonkearns/graphqelm
yarn run watch

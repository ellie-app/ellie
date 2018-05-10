#! /usr/bin/env bash

set -e

cd assets
npm install

until [ -e ../priv/graphql/schema.json ]; do
  >&2 echo "Waiting for graphql schema to be available"
  sleep 1
done

npx graphqelm --introspection-file ../priv/graphql/schema.json --base Ellie.Api --output elm-stuff/generated/dillonkearns/graphqelm
npm run watch

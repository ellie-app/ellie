echo "RUNNING STATIC BUILD"
echo "1) Entering Phoenix directory: $phoenix_dir"
cd $phoenix_dir

echo "2) Generating GraphQL client"
mix ellie.graphql

echo "3) Building client applications"
pushd assets
yarn run build
popd
mix "${phoenix_ex}.digest"

echo "[SKIP] 4) Downloading runtime executables"
# mix ellie.binstall

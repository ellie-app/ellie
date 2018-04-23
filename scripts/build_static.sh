echo "RUNNING STATIC BUILD"
echo "1) Entering Phoenix directory: $phoenix_dir"
cd $phoenix_dir

echo "2) Installing NPM dependencies"
npm --prefix ./assets install

echo "3) Generating GraphQL client"
mix ellie.graphql

echo "4) Building client applications"
npm --prefix ./assets run build
mix "${phoenix_ex}.digest"

echo "5) Downloading runtime executables"
mix ellie.binstall

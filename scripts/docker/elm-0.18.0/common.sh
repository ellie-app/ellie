elm_platform_init() {
    elm-make --yes
    mkdir src
    cat elm-package.json | jq '."source-directories" |= ["src"]' | cat > elm-package.json
}

elm_platform_install() {
    packages = $1
    cat elm-package.json | jq '."dependencies" |= ${packages}' | cat > elm-package.json
}

elm_platform_compile() {
    input = $1
    entry = src/$2
    rm -r src
    mkdir -p entry
    echo $input > entry
    elm-make entry --report json --yes --debug --output build.js
}

.PHONY: help

VERSION ?= `grep 'version' mix.exs | sed -e 's/ //g' -e 's/version://' -e 's/[",]//g'`
IMAGE_NAME ?= ellie
PWD ?= `pwd`
BUILD ?= `git rev-parse --short HEAD`

help:
	@echo "$(IMAGE_NAME):$(VERSION)"
	@perl -nle'print $& if m{^[a-zA-Z_-]+:.*?## .*$$}' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

compile: ## Build the application
	mix do deps.get, compile
	mix do loadpaths, absinthe.schema.json priv/graphql/schema.json
	cd assets && \
		npm install && \
		npm run graphql && \
		npm run build
	mix phx.digest

clean: ## Clean up generated artifacts
	@rm -rf \
		assets/node_modules \
		assets/elm-stuff \
		priv/graphql \
		priv/bin \
	mix clean --deps

image: ## Mimic CodeBuild build
	$(MAKE) clean
	docker run --rm -e BUILD_DIR=/app -v $(PWD):/app -it centos:7 /app/scripts/build all

release: ## Build a release of the application with MIX_ENV=prod
	MIX_ENV=prod mix do deps.get, compile
	MIX_ENV=prod mix do loadpaths, absinthe.schema.json priv/graphql/schema.json
	cd assets && \
		NODE_ENV=production npm install && \
		NODE_ENV=production npm run graphql && \
		NODE_ENV=production npm run build
	MIX_ENV=prod mix phx.digest
	MIX_ENV=prod mix release --verbose --env=prod
	@cp _build/prod/rel/$(IMAGE_NAME)/releases/$(VERSION)/$(IMAGE_NAME).tar.gz $(IMAGE_NAME).tar.gz

bootstrap: ## Setup the app dev
	$(MAKE) clean
	scripts/bootstrap

server: ## Run the app locally for dev
	scripts/server

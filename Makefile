.PHONY: help

VERSION ?= `grep 'version' mix.exs | sed -e 's/ //g' -e 's/version://' -e 's/[",]//g'`
IMAGE_NAME ?= ellie
PWD ?= `pwd`
BUILD ?= `git rev-parse --short HEAD`

help:
	@echo "$(IMAGE_NAME):$(VERSION)"
	@perl -nle'print $& if m{^[a-zA-Z_-]+:.*?## .*$$}' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

init: ## Initialize the project from a clean state
	mix local.rebar --force
	mix local.hex --force
	mix deps.get

compile: ## Build the application
	mix do deps.get, compile
	mix do loadpaths, absinthe.schema.json priv/graphql/schema.json
	cd assets && \
		npm install && \
		npm run graphql && \
		npm run build
	mix phx.digest

clean: ## Clean up generated artifacts
	mix clean

image: ## Mimic CodeBuild build
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

build: ## Build the Docker image
	docker build --build-arg APP_NAME=$(IMAGE_NAME) \
		--build-arg APP_VSN=$(VERSION) \
		-t $(IMAGE_NAME):$(VERSION)-$(BUILD) \
		-t $(IMAGE_NAME):latest .

run: ## Run the app in Docker
	docker run --env-file .env.local \
		--expose 4000 -p 4000:4000 \
		--rm -it $(IMAGE_NAME):latest

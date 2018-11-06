FROM centos:7 AS builder

ARG APP_NAME=ellie
ARG APP_VSN
ARG MIX_ENV=prod
ARG RELEASE_ENV=dev
ARG NODE_ENV=production

WORKDIR /app

COPY . .

RUN scripts/build install

RUN scripts/build pre_build

RUN mix do deps.compile, compile \
    && mix do loadpaths, absinthe.schema.json priv/graphql/schema.json \
    && cd assets \
    && npm install \
    && npm run graphql \
    && npm run build \
    && cd .. \
    && mix phx.digest

RUN mkdir -p /built \
    && mix release --verbose --env=${RELEASE_ENV} \
    && cp _build/${MIX_ENV}/rel/${APP_NAME}/releases/${APP_VSN}/${APP_NAME}.tar.gz /built \
    && cd /built \
    && tar -xzf ${APP_NAME}.tar.gz \
    && rm ${APP_NAME}.tar.gz

FROM amazonlinux:2018.03.0.20180827

ARG APP_NAME=ellie

ENV REPLACE_OS_VARS=true \
    APP_NAME=${APP_NAME}

WORKDIR /app

COPY --from=builder /built .

RUN yum install gcc unzip make tar gzip -y \
    && curl -L https://github.com/obmarg/libsysconfcpus/archive/master.zip --output /usr/local/src/libsysconfcpus.zip \
    && unzip /usr/local/src/libsysconfcpus.zip -d /usr/local/src \
    && cd /usr/local/src/libsysconfcpus-master \
    && ./configure && make && make install

EXPOSE 4000
CMD trap 'exit' INT; \
    /app/bin/${APP_NAME} binstall \
    && /app/bin/${APP_NAME} migrate \
    && /app/bin/${APP_NAME} foreground

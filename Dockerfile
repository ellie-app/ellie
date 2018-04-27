FROM elixir:1.6

ENV DEBIAN_FRONTEND=noninteractive

# Install build-time deps
RUN curl -sL https://deb.nodesource.com/setup_9.x | bash - \
    && curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
    && echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list \
    && apt-get update \
    && apt-get install --no-install-recommends -qy build-essential nodejs yarn

# Install libsysconfcpus
RUN git clone https://github.com/obmarg/libsysconfcpus.git /usr/local/src/libsysconfcpus \
    && cd /usr/local/src/libsysconfcpus \
    && ./configure \
    && make \
    && make install \
    && cd / \
    && sysconfcpus --version

ENV MIX_ENV=prod \
    NODE_ENV=production \
    PORT=4000

# Install Elixir tools
RUN mix local.hex --force \
    && mix local.rebar --force \
    && mix archive.install https://github.com/phoenixframework/archives/raw/master/phoenix_new.ez --force

# Download Elm platform binaries
RUN mkdir -p /tmp/elm_bin/0.18.0 && mkdir /tmp/elm_bin/0.19.0 \
    # goon executable for Procelain Elixir library, to run executables in Elixir processes
    && wget -q https://github.com/alco/goon/releases/download/v1.1.1/goon_linux_386.tar.gz -O /tmp/goon.tar.gz \
    && tar -xvC /tmp/elm_bin -f /tmp/goon.tar.gz \
    && chmod +x /tmp/elm_bin/goon \
    && rm /tmp/goon.tar.gz \
    # Elm Platform 0.18
    && wget -q https://github.com/elm-lang/elm-platform/releases/download/0.18.0-exp/elm-platform-linux-64bit.tar.gz -O /tmp/platform-0.18.0.tar.gz \
    && tar -xvC /tmp/elm_bin/0.18.0 -f /tmp/platform-0.18.0.tar.gz \
    && rm /tmp/platform-0.18.0.tar.gz \
    # Elm Format 0.18
    && wget -q https://github.com/avh4/elm-format/releases/download/0.7.0-exp/elm-format-0.18-0.7.0-exp-linux-x64.tgz -O /tmp/format-0.18.0.tar.gz \
    && tar -xvC /tmp/elm_bin/0.18.0 -f /tmp/format-0.18.0.tar.gz \
    && rm /tmp/format-0.18.0.tar.gz \
    && chmod +x /tmp/elm_bin/0.18.0/* \
    # Elm Platform 0.19
    && wget -q ***REMOVED*** -O /tmp/elm_bin/0.19.0/elm \
    && chmod +x /tmp/elm_bin/0.19.0/elm \
    # Elm Format 0.19 - TODO download elm-format 0.19.0 when it's ready
    && cp /tmp/elm_bin/0.18.0/elm-format /tmp/elm_bin/0.19.0/elm-format

# Load source code
ADD . /app
WORKDIR /app

# Copy binaries and set up ELM_HOME
ENV ELM_HOME=/app/priv/elm_home
RUN mkdir -p /app/priv/bin \
    && cp -r /tmp/elm_bin/* /app/priv/bin \
    && mkdir -p /app/priv/elm_home

# Compile Elixir code and generate schema
RUN mix deps.get \
    && mix compile \
    && mix do loadpaths, absinthe.schema.json /app/priv/graphql/schema.json

# Install node dependencies, compile production Elm apps, and generate digested assets
RUN cd /app/assets \
    && yarn install \
    && yarn run graphql \
    && yarn run build \
    && cd /app \
    && mix phx.digest

# Run the server
EXPOSE 4000
CMD mix phx.server

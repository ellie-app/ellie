FROM frolvlad/alpine-glibc

# GHC needs libgmp
RUN apk add --no-cache gmp

# Download Elm Platform
RUN wget https://github.com/elm-lang/elm-platform/releases/download/0.18.0-exp/elm-platform-linux-64bit.tar.gz -O elm-platform.tar.gz \
    && tar xzvC /usr/local/bin -f elm-platform.tar.gz \
    && rm elm-platform.tar.gz \
    && chmod +x /usr/local/bin/elm*

# Download Elm Format
RUN wget https://github.com/avh4/elm-format/releases/download/0.7.0-exp/elm-format-0.18-0.7.0-exp-linux-x64.tgz -O elm-format.tar.gz \
    && tar xzvC /usr/local/bin -f elm-format.tar.gz \
    && rm elm-format.tar.gz \
    && chmod +x /usr/local/bin/elm-format

WORKDIR /workspace

COPY ./common.sh /common.sh
RUN . /common.sh

# NOTE: probably don't need this, can build from Makefile
FROM amazonlinux:latest

ENV HOME=/app

WORKDIR /app

CMD ["/bin/bash"]

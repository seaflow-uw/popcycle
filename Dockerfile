ARG BASE_VERSION=latest
FROM ctberthiaume/popcycle-base:${BASE_VERSION}

COPY . /src
WORKDIR /src

RUN git clean -fdx \
&& rm -rf .git \
&& Rscript install-package-only.R

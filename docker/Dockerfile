ARG BASE_VERSION=latest
FROM ctberthiaume/popcycle-base:${BASE_VERSION}

COPY . /src
WORKDIR /src

RUN Rscript install-package-only.R

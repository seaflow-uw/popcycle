#!/bin/bash
# Build popcycle Docker image
# Any arguments will be passed on to docker build, e.g. you can set --build-arg

set -e

version=$(awk '$1 == "Version:" {print $2}' DESCRIPTION)
tag="popcycle:${version}"
echo "Building ${tag}"
awk '$1 == "Version:" {print $1, "0.1.0"; next} {print $0}' DESCRIPTION >DESCRIPTION_docker
docker build "$@" --progress=plain -t "${tag}" .
docker run -it --rm "${tag}" Rscript tests/testthat.R

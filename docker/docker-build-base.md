# Popcycle Docker image

The commands in this document expect that the present working directory
is the `docker` subdirectory of this repo.

## Base Build

The base docker image contains system library deps, R, and R package deps, and
seaflowpy as a single-file executable.
Since this image is large and takes a while to compile it should be updated
infrequently.

```
docker build --build-arg MAKE_JOBS=4 -t popcycle-base:$(date +%Y-%m-%d) -f Dockerfile-base ..
```

This builds a new image and tells make to try to run 4 compilation jobs at once.

## Popcycle build

This image builds on the base image and installs the popcycle package. It does
not update any depdencies installed in the base image.
The idea is to make it possible to pull a thin image layer for just popcycle
updates while at sea, assuming the base image is already present and doesn't
require udpates.

```
# Get latest commit hash for image tag
COMMIT=$(git describe | awk '{split($1, s, "-"); print substr(s[length(s)], 2)}')

docker build -t popcycle:${COMMIT} -f Dockerfile ..
```

ARG ARG_BIOC_VERSION=RELEASE_3_19
FROM bioconductor/bioconductor_docker:$ARG_BIOC_VERSION

RUN apt-get update && apt-get install -y \
    cmake \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgeos-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libpng-dev \
    libtiff5-dev \
    libxml2-dev \
    sqlite3 \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*


#
WORKDIR /popcycle/

# Install dependencies in its own layer
COPY DESCRIPTION_docker DESCRIPTION
COPY setup.R setup.R

ARG DEPS_ONLY=TRUE
ARG INSTALL_NCPUS=2
RUN Rscript setup.R

# Install the package itself
COPY DESCRIPTION LICENSE make_docs.R NAMESPACE README.md ./
COPY ./R /popcycle/R
COPY ./executable_scripts ./executable_scripts
COPY ./man ./man
COPY ./inst ./inst
COPY ./tests ./tests

RUN R CMD INSTALL .

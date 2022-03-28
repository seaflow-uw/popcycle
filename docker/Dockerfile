FROM rocker/rstudio:4.1.2

RUN apt-get update && apt-get install -y \
    cmake \
    libgeos-dev \
    libpng-dev \
    libxml2-dev \
    sqlite3 \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*


# Ncpus argument devtools::install*() or install.packages() to allow parallel
# installation
ARG INSTALL_NCPUS=4
# Make sure arrow tries to use pre-built binary libraries with compression
# support.
ARG LIBARROW_BINARY=true

# Install dependencies in its own layer
ARG DEPS_ONLY=true
COPY renv.lock /popcycle/
WORKDIR /popcycle/

ENV RENV_VERSION 0.15.4
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
RUN R -e "renv::restore()"

# Install the package itself
COPY DESCRIPTION LICENSE make_docs.R NAMESPACE README.md /popcycle/
COPY ./R /popcycle/R
COPY ./executable_scripts /popcycle/executable_scripts
COPY ./man /popcycle/man
COPY ./inst /popcycle/inst
COPY ./tests /popcycle/tests

RUN R CMD INSTALL .

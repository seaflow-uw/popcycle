FROM bioconductor/bioconductor_docker:RELEASE_3_14

RUN apt-get update \
    && apt-get install sqlite3 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*


# Ncpus argument devtools::install*() or install.packages() to allow parallel
# installation
ARG INSTALL_NCPUS=4
# Make sure arrow tries to use pre-built binary libraries with compression
# support.
ARG LIBARROW_BINARY=true

# R user and user R library location
ARG ARG_USER=rstudio

USER ${ARG_USER}

# Install dependencies in its own layer
ARG DEPS_ONLY=true
COPY --chown=${ARG_USER}:${ARG_USER} DESCRIPTION setup.R /popcycle/
WORKDIR /popcycle/

# Install flowDensity and flowCore separately because automatic installation
# from DESCRIPTION was failing with the 3.13 and 3.14 bioconductor docker images
RUN Rscript --slave -e 'BiocManager::install(c("flowCore", "flowDensity"))' \
    && Rscript setup.R

# Install the package itself
ARG DEPS_ONLY=false
COPY --chown=${ARG_USER}:${ARG_USER} LICENSE make_docs.R NAMESPACE README.md /popcycle/
COPY --chown=${ARG_USER}:${ARG_USER} ./R /popcycle/R
COPY --chown=${ARG_USER}:${ARG_USER} ./executable_scripts /popcycle/executable_scripts
COPY --chown=${ARG_USER}:${ARG_USER} ./man /popcycle/man
COPY --chown=${ARG_USER}:${ARG_USER} ./inst /popcycle/inst
COPY --chown=${ARG_USER}:${ARG_USER} ./tests /popcycle/tests

RUN Rscript setup.R

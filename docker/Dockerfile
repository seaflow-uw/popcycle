FROM bioconductor/bioconductor_docker:RELEASE_3_12


# Install only dependencies
ARG DEPS_ONLY=false
# Ncpus argument devtools::install*() or install.packges() to allow parallel
# installation
ARG INSTALL_NCPUS=4
# Make sure arrow tries to use pre-built binary libraries with compression
# support.
ARG LIBARROW_BINARY=true

# R user and user R library location
ARG ARG_USER=rstudio
ENV R_LIBS_USER=/home/${ARG_USER}/R_libs

COPY --chown=${ARG_USER}:${ARG_USER} DESCRIPTION LICENSE make_docs.R NAMESPACE README.md setup.R /popcycle/
COPY --chown=${ARG_USER}:${ARG_USER} ./R /popcycle/R
COPY --chown=${ARG_USER}:${ARG_USER} ./executable_scripts /popcycle/executable_scripts
COPY --chown=${ARG_USER}:${ARG_USER} ./man /popcycle/man
COPY --chown=${ARG_USER}:${ARG_USER} ./inst /popcycle/inst
COPY --chown=${ARG_USER}:${ARG_USER} ./tests /popcycle/tests

USER ${ARG_USER}
RUN mkdir ${R_LIBS_USER}
WORKDIR /popcycle/
RUN Rscript setup.R

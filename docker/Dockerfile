FROM bioconductor/bioconductor_docker:RELEASE_3_11

# Install system dependencies for popcycle R package installation
# Separating this image from the image that installs popcycle + deps allows for
# smaller pulls after reinstalling new popcycle versions.

ARG ARG_USER=rstudio

ENV R_LIBS_USER=/home/${ARG_USER}/R_libs
# Ncpus argument devtools::install*() or install.packges()
ARG INSTALL_NCPUS=4
# Allow parallel compilation
# Make sure arrow uses pre-built binary libraries if possible and builds with
# compression support.
ARG LIBARROW_BINARY=true
ARG LIBARROW_MINIMAL=false

COPY --chown=${ARG_USER}:${ARG_USER} DESCRIPTION LICENSE make_docs.R NAMESPACE README.md setup.R setup-deps-only.R /popcycle/
COPY --chown=${ARG_USER}:${ARG_USER} ./R /popcycle/R
COPY --chown=${ARG_USER}:${ARG_USER} ./executable_scripts /popcycle/executable_scripts
COPY --chown=${ARG_USER}:${ARG_USER} ./man /popcycle/man
COPY --chown=${ARG_USER}:${ARG_USER} ./inst /popcycle/inst
COPY --chown=${ARG_USER}:${ARG_USER} ./tests /popcycle/tests

USER ${ARG_USER}
RUN mkdir ${R_LIBS_USER}
WORKDIR /popcycle/
RUN Rscript setup.R

FROM r-base:3.6.0

COPY . /src

WORKDIR /src

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libgit2-dev \
    libssh2-1-dev \
    libssl-dev \
    file \
    libxml2-dev \
    libgeos-dev \
    curl \
    git \
    sqlite3 \
&& rm -rf /var/lib/apt/lists/*

RUN curl -sLO  https://github.com/armbrustlab/seaflowpy/releases/latest/download/seaflowpy-linux64 \
&& chmod +x seaflowpy-linux64 \
&& mv seaflowpy-linux64 /usr/local/bin/seaflowpy

RUN git clean -fdx \
&& rm -rf .git \
&& Rscript setup.R

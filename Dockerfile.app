FROM rocker/tidyverse:4.4.2

# System deps for lightgbm and friends
RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
      build-essential \
      cmake \
      libssl-dev \
      libcurl4-openssl-dev \
      libgit2-dev \
      libgomp1 \
      libsodium-dev && \
    rm -rf /var/lib/apt/lists/*

ENV RENV_PATHS_CACHE=/root/.cache/R/renv
WORKDIR /app

COPY renv.lock renv.lock
COPY renv/ renv/
RUN R -q -e "install.packages('renv', repos = 'https://cloud.r-project.org'); renv::restore(clean = TRUE, repos = c(CRAN = 'https://packagemanager.posit.co/cran/latest'))"
RUN R -q -e "install.packages(c('shiny','lightgbm','probably'), repos = 'https://cloud.r-project.org')"

COPY models ./models
COPY scripts ./scripts
COPY data ./data

EXPOSE 8000

ENTRYPOINT ["R", "-q", "-e", "shiny::runApp('scripts/prediction_app.R', host='0.0.0.0', port=8000)"]

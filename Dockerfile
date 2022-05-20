FROM rocker/rstudio

RUN apt-get update \
    && apt-get -y install libcurl4-openssl-dev \
    libssl-dev \
    libxt6 \
    libxml2-dev \
    libz-dev\
    tar

RUN R -e "install.packages('devtools')"
COPY . /HRAexplorer/
RUN R -e "devtools::install_github('jaropis/hrvhra', dependencies = TRUE)"
RUN R -e "devtools::install_github('jaropis/shinydashboardplus0.7.5')"
RUN Rscript -e "devtools::install_deps('/HRAexplorer/', upgrade = 'default')"
RUN R -e "install.packages('/HRAexplorer/', repos = NULL, type = 'source')"

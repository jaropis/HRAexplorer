FROM rocker/rstudio

RUN apt-get update \
    && apt-get -y install libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libz-dev\
    tar

RUN R -e "install.packages('devtools')"
COPY . /HRAexplorer/
RUN R -e "devtools::install_github('jaropis/hrvhra', dependencies = TRUE)"
RUN Rscript -e "devtools::install_deps('/HRAexplorer/')"
RUN R -e "install.packages('/HRAexplorer/', repos = NULL, type = 'source')"
# RUN cp /HRAexplorer/Deployment/Renviron /HRAExplorer/.Renviron
RUN cp /HRAExplorer/Rprofile.site /usr/local/lib/R/etc
RUN mkdir -p /usr/lib/R/etc # this folder is required by shinyProxy
RUN cp -p /usr/local/lib/R/etc/* /usr/lib/R/etc
EXPOSE 3838

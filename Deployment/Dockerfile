FROM rocker/rstudio

RUN apt-get update \
    && apt-get -y install libcurl4-openssl-dev \
    libssl-dev \
    libxt6 \
    libxml2-dev \
    libz-dev\
    tar\
    libudunits2-dev

RUN R -e "install.packages('devtools')"
COPY . /HRAexplorer/
RUN R -e "devtools::install_github('jaropis/hrvhra', dependencies = TRUE)"
RUN R -e "devtools::install_github('jaropis/shinydashboardplus0.7.5')"
RUN Rscript -e "devtools::install_deps('/HRAexplorer/', upgrade = 'default')"
RUN R -e "install.packages('/HRAexplorer/', repos = NULL, type = 'source')"
# RUN cp /HRAexplorer/Deployment/Renviron /HRAexplorer/.Renviron
RUN cp /HRAexplorer/Rprofile.site /usr/local/lib/R/etc
RUN mkdir -p /usr/lib/R/etc # this folder is required by shinyProxy
RUN cp -p /usr/local/lib/R/etc/* /usr/lib/R/etc
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/HRAexplorer/inst/shiny', port = 3838, host = '0.0.0.0')]"


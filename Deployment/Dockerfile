FROM rocker/rstudio

RUN apt-get update \
    && apt-get -y install libcurl4-openssl-dev \
    libssl-dev \
    libxt6 \
    libxml2-dev \
    libz-dev\
    tar\
    libudunits2-dev \
    build-essential \
    curl \
    gcc \
    pkg-config

# installing Rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

# installing cargo-c (needed for C bindings)
RUN cargo install cargo-c

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
RUN cd HRAexplorer
CMD ["R", "-e", "shiny::runApp('/HRAexplorer/inst/shiny', port = 3838, host = '0.0.0.0')"]


FROM rocker/rstudio

RUN apt-get update \
    && apt-get -y install default-jre \
    default-jdk \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && R CMD javareconf \
    && apt-get -y install r-cran-rjava

RUN install2.r shiny \
               DT \
               shiny \
               shinydashboard \
               shinydashboardPlus \
               XLConnect \
               devtools \
               remotes \
               roxygen2

RUN installGithub.r   jaropis/hrvhra

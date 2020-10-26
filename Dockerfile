FROM rocker/rstudio

RUN apt-get update \
    && apt-get -y install libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libz-dev\
    tar

RUN install2.r shiny \
               DT \
               shiny \
               shinydashboard \
               shinydashboardPlus \
               XLConnect \
               devtools \
               remotes \
               roxygen2 \
               dplyr

# ARG not to use cache at this point
ARG HRA_VER=unknown
RUN installGithub.r   jaropis/hrvhra

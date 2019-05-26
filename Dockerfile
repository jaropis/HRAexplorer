FROM rocker/rstudio

RUN apt-get update \
    && apt-get -y install default-jre \
    && apt-get -y install default-jdk \
    && R CMD javareconf \
    && apt-get -y install r-cran-rjava 
    
RUN install2.r shiny \
               DT \
               shiny \
               shinydashboard \
               shinydashboardPlus \
               XLConnect \
               devtools \
               remotes
               
RUN installGithub.r jaropis/hrvhra
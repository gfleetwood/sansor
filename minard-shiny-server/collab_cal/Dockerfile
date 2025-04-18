FROM rocker/shiny:4.1.0

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    unixodbc-dev \
    odbc-postgresql

RUN R -e 'install.packages(c("tidyverse", "rhandsontable", "reactable", "lubridate", "odbc", "DBI", "pool"), \
          repos = "https://packagemanager.rstudio.com/cran/__linux__/focal/2021-04-23")'
          
WORKDIR /home/app
COPY app.R .
ADD ./odbcinst_app.ini /etc/odbcinst.ini

#EXPOSE 3838
#CMD ["R", "-e", "shiny::runApp('/home/app', host = '0.0.0.0', port = 3838)"]

ENV PORT=3838

CMD ["R", "-e", "shiny::runApp('/home/app', host = '0.0.0.0', port= as.numeric(Sys.getenv('PORT')))"]



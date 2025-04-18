FROM rocker/shiny:4.0.5 

RUN apt update && \
    apt install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    unixodbc-dev \
    odbc-postgresql

RUN R -e 'install.packages( \
          c("leaflet", "plotly", "tidyverse", "stringi", "knitr", "odbc", "DBI", "pool", "glue", "DT", "diffr"), \
          dependencies = T, \
          repos = "https://packagemanager.rstudio.com/all/__linux__/focal/2022-10-06+Y3JhbiwyOjQ1MjYyMTU7MjdGNzBGNTE" \
          )'

COPY . /srv/shiny-server/
#RUN rm -f /srv/shiny-server/index.html
#RUN Rscript -e 'knitr::knit("/srv/shiny-server/index.Rmd", output = "/srv/shiny-server/index.html")'
#rUN rm -f index.Rmd

EXPOSE 3838

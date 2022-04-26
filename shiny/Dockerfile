# https://blog.sellorm.com/2021/04/25/shiny-app-in-docker/

# get shiny server and R from the rocker project
FROM rocker/shiny:4.0.5

# system libraries
# Try to only install system libraries you actually need
# Package Manager is a good resource to help discover system deps
RUN apt-get update && apt-get install -y vim \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    r-cran-tidyr \
    r-cran-shiny \
    r-cran-ggplot2 \
    r-cran-survival \
    r-cran-lme4 \
    r-cran-plotly \
    libgdal-dev

# install R packages required
RUN R -e "install.packages(c(\
              'survminer', \
              'RMariaDB', \
              'ggwordcloud', \
              'quanteda', \
              'quanteda.textstats', \
              'DT', \
              'devtools', \
              'timevis', \
              'pkgconfig', \
              'lmtest', \
              'timevis', \
              'leaflet', \
              'sp' \
            ), \
            repos='https://packagemanager.rstudio.com/cran/__linux__/focal/2021-04-23' \
          )"

RUN R -e "devtools::install_github('ironholds/WikipediR')"
RUN R -e "devtools::install_github('bhaskarvk/leaflet.extras')"

# copy the app directory into the image
COPY ./shiny-server/ /srv/shiny-server/

# run app
CMD ["su", "-", "shiny", "-c", "/usr/bin/shiny-server"]
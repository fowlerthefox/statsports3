# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest

# system libraries of general use
RUN echo 'deb http://deb.debian.org/debian bullseye main' > /etc/apt/sources.list
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libcurl4-openssl-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libssh2-1-dev \
    libcairo2-dev \
    sudo
   
  
# install R packages required 
# (change it dependeing on the packages you need)
RUN R -e "install.packages(c('pool','plotly', 'leaflet','leaflet.extras', 'shiny', 'DBI', 'RPostgreSQL', 'sf', 'geosphere', 'promises', 'future', 'remotes', 'ggthemes', 'ggsoccer'))"
RUN R -e "remotes::install_github('rstudio/bootstraplib')"

# copy the app to the image
COPY app.R /srv/shiny-server/
COPY filters.R /srv/shiny-server/
COPY functions.R /srv/shiny-server/
COPY tab-1.R /srv/shiny-server/
COPY tab-2.R /srv/shiny-server/
COPY tab-3.R /srv/shiny-server/
COPY shiny-server.sh /usr/bin/shiny-server.sh

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server
# run app
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]
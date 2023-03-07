FROM rocker/tidyverse:latest
## Install Java
RUN apt-get update && apt-get install -y \
default-jdk \
## used to build rJava and other packages
libbz2-dev \
libicu-dev \
liblzma-dev \
libsodium-dev
RUN install2.r --error -n 4 -r http://cran.rstudio.org Rcpp plumber DBI RMariaDB jsonlite config jose plotly RCurl stringdist xlsx easyPubMed rvest lubridate pool memoise coop cowplot
## same as:
#RUN echo "options(repos = c(CRAN = 'http://cran.rstudio.org'))" >> /usr/local/lib/R/etc/Rprofile.site
#RUN R -e "install.packages(c('plumber', 'Rcpp', 'DBI', 'RMariaDB', 'jsonlite', 'config', 'jose', 'plotly', 'RCurl', 'stringdist', 'xlsx', 'easyPubMed', 'rvest', 'lubridate', 'pool', 'memoise', 'coop', 'cowplot'), method = 'wget')"

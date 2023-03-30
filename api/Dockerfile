FROM rocker/tidyverse:4.2.1
RUN apt-get update && apt-get install -y \
## Install Java
default-jdk \
## used for keyring package
libsecret-1-dev \
## used to build rJava and other packages
libbz2-dev \
libicu-dev \
liblzma-dev \
libsodium-dev \
libtool
RUN install2.r --error -n 4 -r http://cran.rstudio.org Rcpp plumber DBI RMariaDB jsonlite config jose plotly RCurl stringdist xlsx easyPubMed rvest lubridate pool memoise coop cowplot keyring rlang
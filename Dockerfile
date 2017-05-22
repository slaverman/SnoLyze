FROM rocker/rstudio-stable:latest
MAINTAINER x@y.com

RUN apt-get update -qq && apt-get install -y \
  git-core \
  libssl-dev \
  libcurl4-gnutls-dev \
  libv8-dev 

RUN R -e 'install.packages(c("devtools"))'

RUN R -e 'devtools::install_github("slaverman/SnoLyze")'


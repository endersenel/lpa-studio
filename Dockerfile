FROM rocker/shiny:latest

# Linux sistem kütüphanelerinin kurulumu (Web, cURL, SSL ve XML bağımlılıkları)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# R Paketlerinin Kurulumu (Eksik httr ve texreg dahil)
RUN R -e "install.packages(c('httr', 'texreg', 'shinydashboard', 'haven', 'readxl', 'tidyLPA', 'dplyr', 'ggplot2', 'nnet', 'DT', 'tidyr'), repos='https://cloud.r-project.org/')"

# Uygulama dosyasını kopyala
COPY app.R /srv/shiny-server/app.R

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]

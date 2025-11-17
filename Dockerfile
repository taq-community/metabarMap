FROM rocker/verse:4.5.1
RUN apt-get update && apt-get install -y  cmake gdal-bin libabsl-dev libgdal-dev libgeos-dev libicu-dev libpng-dev libproj-dev libsqlite3-dev libssl-dev libudunits2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.8.1")'
RUN Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.9.0")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.4")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.11.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("zip",upgrade="never", version = "2.3.3")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("reactable",upgrade="never", version = "0.4.4")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.2.3")'
RUN Rscript -e 'remotes::install_version("markdown",upgrade="never", version = "1.13")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.5.1")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 3838
CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');library(metabarMap);metabarMap::run_app()"]

# Use rocker/shiny as base image
FROM rocker/shiny:4.3.2

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c(\
    'shiny', \
    'bslib', \
    'leaflet', \
    'reactable', \
    'dplyr', \
    'tidyr', \
    'golem', \
    'htmltools', \
    'zip' \
    ), repos='https://cloud.r-project.org/')"

# Create app directory
RUN mkdir -p /srv/shiny-server/metabarMap

# Copy app files
COPY . /srv/shiny-server/metabarMap/

# Set working directory
WORKDIR /srv/shiny-server/metabarMap

# Expose port
EXPOSE 3838

# Run app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/metabarMap', host='0.0.0.0', port=3838)"]

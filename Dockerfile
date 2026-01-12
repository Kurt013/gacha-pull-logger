# Use the official Shiny image with R 4.3.2
FROM rocker/shiny:4.3.2

# Install system libraries required by your R packages
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*

# Install only the packages your app actually uses
RUN R -e "install.packages(c( \
    'shiny', \
    'shinyjs', \
    'shinytoastr', \
    'DT', \
    'DBI', \
    'RSQLite', \
    'plotly', \
    'readxl', \
    'dplyr' \
    'httr', \
    'jsonlite' \
), repos = 'https://cloud.r-project.org')"

# Copy your app to the root of Shiny Server so it's served at /
COPY . /srv/shiny-server/

# Ensure Shiny Server can read/write everything it needs
RUN chown -R shiny:shiny /srv/shiny-server

# Expose Shiny Server's port (Render expects 3838)
EXPOSE 3838

# Start Shiny Server
CMD ["/usr/bin/shiny-server"]
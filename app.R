# Load required libraries
library(shiny)
library(bslib)
library(leaflet)
library(reactable)
library(dplyr)
library(tidyr)

# Source modules and app files
source("R/mod_map.R")
source("R/mod_species_table.R")
source("R/app_ui.R")
source("R/app_server.R")

# Define UI and Server
ui <- app_ui(request = NULL)
server <- app_server

# Run the application
shinyApp(ui = ui, server = server)

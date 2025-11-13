#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Add resource path for images
  shiny::addResourcePath("img", "data/img")

  # Load data
  localisations <- read.csv("data/localisations.csv", stringsAsFactors = FALSE)
  species_data_raw <- read.csv("data/species_table.csv", stringsAsFactors = FALSE)

  # Prepare data: calculate species richness per station
  # Regular species (excluding ambiguous groups)
  species_data <- species_data_raw |>
    dplyr::filter(TaxonName != "sample") |>
    dplyr::filter(Group != "MultipleHits") |>
    dplyr::filter(Group != "zMultiple") |>
    dplyr::select(Group, Genus, Species, dplyr::all_of(get_golem_config("station_ids"))) |>
    tidyr::pivot_longer(dplyr::all_of(get_golem_config("station_ids")), names_to = "station_id", values_to = "n_reads") |>
    dplyr::filter(n_reads > 0)

  # Ambiguous groups (zMultiple)
  ambiguous_data <- species_data_raw |>
    dplyr::filter(TaxonName != "sample") |>
    dplyr::filter(Group == "zMultiple") |>
    dplyr::select(Group, Genus, Species, dplyr::all_of(get_golem_config("station_ids"))) |>
    tidyr::pivot_longer(dplyr::all_of(get_golem_config("station_ids")), names_to = "station_id", values_to = "n_reads") |>
    dplyr::filter(n_reads > 0)

  species_count_by_station <- species_data |>
    dplyr::group_by(station_id) |>
    dplyr::summarise(n_species = dplyr::n_distinct(Genus, Species))

  # Merge location data with species richness
  map_data <- localisations |>
    dplyr::right_join(species_count_by_station, by = c("station" = "station_id"))

  # Map module - returns selected station
  selected_station <- mapServer("map_module", map_data)

  # Species table module
  speciesTableServer("species_table_module", species_data, ambiguous_data, selected_station)

  # Update URL when station changes
  shiny::observe({
    station <- selected_station()
    if (!is.null(station)) {
      shiny::updateQueryString(paste0("?station=", station), mode = "push")
    }
  })

  # Restore station from URL on load
  shiny::observe({
    query <- shiny::parseQueryString(session$clientData$url_search)
    if (!is.null(query$station) && query$station %in% map_data$station) {
      selected_station(query$station)
    }
  })

  # Download all CSV files handler
  output$download_all_data <- shiny::downloadHandler(
    filename = function() {
      paste0("metabarMap_all_data_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Get all CSV files in data folder
      csv_files <- list.files("data", pattern = "\\.csv$", full.names = TRUE)

      # Create temporary directory
      temp_dir <- tempdir()
      temp_files <- file.path(temp_dir, basename(csv_files))

      # Copy CSV files to temp directory
      file.copy(csv_files, temp_files, overwrite = TRUE)

      # Create zip file
      zip::zip(file, files = basename(csv_files), root = temp_dir)
    }
  )
}

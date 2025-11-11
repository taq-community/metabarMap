#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Load data
  localisations <- read.csv("data/localisations.csv", stringsAsFactors = FALSE)
  species_data_raw <- read.csv("data/species_table.csv", stringsAsFactors = FALSE)

  # Prepare data: calculate species richness per station
  species_data <- species_data_raw |>
    dplyr::filter(TaxonName != "sample") |>
    dplyr::filter(Group != "MultipleHits") |>
    dplyr::select(Group, Genus, Species, dplyr::all_of(get_golem_config("station_ids"))) |>
    tidyr::pivot_longer(dplyr::all_of(get_golem_config("station_ids")), names_to = "station_id", values_to = "n_reads") |>
    dplyr::filter(n_reads > 0) |>
    # TODO Have to be removed
    dplyr::filter(Group != "zMultiple")
  
  species_count_by_station <- species_data |>
    dplyr::group_by(station_id) |>
    dplyr::summarise(n_species = dplyr::n_distinct(Genus, Species))

  # Merge location data with species richness
  map_data <- localisations |>
    dplyr::right_join(species_count_by_station, by = c("station" = "station_id"))

  # Map module - returns selected station
  selected_station <- mapServer("map_module", map_data)

  # Species table module
  speciesTableServer("species_table_module", species_data, selected_station)
}

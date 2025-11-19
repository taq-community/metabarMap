#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Get path to installed package data
  app_sys <- function(...) {
    system.file(..., package = "metabarMap")
  }

  # Add resource path for images
  shiny::addResourcePath("img", app_sys("extdata", "img"))

  # Load data
  localisations <- read.csv(app_sys("extdata", "localisations.csv"), stringsAsFactors = FALSE)
  species_data_raw <- read.csv(app_sys("extdata", "species_table.csv"), stringsAsFactors = FALSE)

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

  # Render partner logos
  output$partner_logos <- shiny::renderUI({
    # Get partners configuration
    partners <- get_golem_config("partners")

    # Create image tags for each partner
    logo_tags <- lapply(partners, function(partner) {
      shiny::tags$a(
        href = partner$url,
        target = "_blank",
        shiny::tags$img(
          src = partner$image,
          alt = partner$name,
          style = paste0("max-height: ", partner$max_height, "; margin: 0 10px; vertical-align: middle;")
        )
      )
    })

    shiny::tags$div(
      style = "display: flex; align-items: center; height: 100%; gap: 5px;",
      shiny::tags$span(
        "In collaboration with",
        style = "font-size: 1em; margin-right: 10px; color: #666;"
      ),
      logo_tags
    )
  })

  # Toggle methods sidebar
  shiny::observeEvent(input$toggle_methods, {
    bslib::sidebar_toggle("methods_sidebar")
  })

  # Render methods content
  output$methods_content <- shiny::renderUI({
    # Read methodology markdown file
    methodology_file <- get_golem_config("methodology_file")
    md_path <- app_sys("extdata", methodology_file)

    shiny::includeMarkdown(md_path)
  })

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
      # Get all CSV files in extdata folder
      csv_files <- list.files(app_sys("extdata"), pattern = "\\.csv$", full.names = TRUE)

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

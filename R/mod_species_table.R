# Species Table Module UI
speciesTableUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      style = "margin-bottom: 10px; display: flex; justify-content: space-between; align-items: center;",
      shiny::downloadButton(
        ns("download_data"),
        "Download CSV",
        icon = shiny::icon("download"),
        class = "btn-sm"
      )
    ),
    reactable::reactableOutput(ns("species_table"))
  )
}

# Species Table Module Server
speciesTableServer <- function(id, species_data, selected_station) {
  shiny::moduleServer(id, function(input, output, session) {

    # Dynamic card header
    output$card_header <- shiny::renderUI({
      station <- selected_station()
      if (is.null(station)) {
        "Species Detected"
      } else {
        shiny::tagList(
          "Species detected in ",
          shiny::span(station, style = "color: #0066CC;")
        )
      }
    })

    # Create filtered species table
    filtered_data <- shiny::reactive({
      station <- selected_station()

      if (is.null(station)) {
        return(data.frame(
          Group = character(),
          Genus = character(),
          Species = character(),
          Count = integer(),
          stringsAsFactors = FALSE
        ))
      }

      # Use station name directly (already matches column names in CSV)
      station_col <- station

      # Check if column exists in species_data
      if (!station_col %in% names(species_data)) {
        return(data.frame(
          Group = character(),
          Genus = character(),
          Species = character(),
          Count = integer(),
          stringsAsFactors = FALSE
        ))
      }

      # Filter and prepare data
      species_data |>
        dplyr::select(Group, Genus, Species, Count = dplyr::all_of(station_col)) |>
        dplyr::filter(Count > 0) |>
        dplyr::arrange(dplyr::desc(Count))
    })

    # Render reactable
    output$species_table <- reactable::renderReactable({
      reactable::reactable(
        filtered_data(),
        searchable = TRUE,
        pagination = TRUE,
        defaultPageSize = 15,
        highlight = TRUE,
        bordered = TRUE,
        striped = TRUE,
        compact = TRUE,
        columns = list(
          Group = reactable::colDef(
            name = "Group",
            minWidth = 100
          ),
          Genus = reactable::colDef(
            name = "Genus",
            minWidth = 120,
            style = list(fontStyle = "italic")
          ),
          Species = reactable::colDef(
            name = "Species",
            minWidth = 120,
            style = list(fontStyle = "italic")
          ),
          Count = reactable::colDef(
            name = "Read Count",
            format = reactable::colFormat(separators = TRUE),
            minWidth = 100,
            align = "right",
            style = function(value) {
              color <- if (value > 10000) {
                "#00A000"
              } else if (value > 1000) {
                "#FFA500"
              } else {
                "#666666"
              }
              list(color = color, fontWeight = "bold")
            }
          )
        )
      )
    })

    # Download handler
    output$download_data <- shiny::downloadHandler(
      filename = function() {
        station <- selected_station()
        if (is.null(station)) {
          paste0("metabarMap_all_species_", Sys.Date(), ".csv")
        } else {
          paste0("metabarMap_", station, "_", Sys.Date(), ".csv")
        }
      },
      content = function(file) {
        utils::write.csv(filtered_data(), file, row.names = FALSE)
      }
    )
  })
}

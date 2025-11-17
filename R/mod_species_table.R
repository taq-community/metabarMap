# Species Table Module UI
speciesTableUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_card_tab(
    title = shiny::uiOutput(ns("card_title")),
    height = 600,
    full_screen = TRUE,
    bslib::nav_panel(
      bslib::card_title("Detected", class = "text-md"),
      reactable::reactableOutput(ns("species_table"))
    ),
    bslib::nav_panel(
      bslib::card_title("Ambiguous", class = "text-md"),
      reactable::reactableOutput(ns("ambiguous_table"))
    )
  )
}

# Species Table Module Server
speciesTableServer <- function(id, species_data, ambiguous_data, selected_station) {
  shiny::moduleServer(id, function(input, output, session) {

    # Load species info
    species_info <- read.csv(system.file("extdata", "species_info.csv", package = "metabarMap"), stringsAsFactors = FALSE)

    # Dynamic card title
    output$card_title <- shiny::renderUI({
      station <- selected_station()
      if (!is.null(station)) {
        shiny::tags$h5(
          "Species in ",
          shiny::span(station, style = "color: #0066CC;")
        )
      } else {
        shiny::tags$h5("Species")
      }
    })

    # Create filtered species table
    filtered_data <- shiny::reactive({
      station <- selected_station()
      # Filter and prepare data
      species_data |>
        dplyr::filter(station == station_id) |>
        dplyr::mutate(scientific_name = paste(Genus, Species)) |>
        dplyr::left_join(
          species_info |>
            dplyr::select(species, English, French, gbif_url, col_link, itis_url,
                         Native_Quebec, Exotic_Quebec, img),
          by = c("scientific_name" = "species")
        ) |>
        dplyr::mutate(
          Status = dplyr::case_when(
            Exotic_Quebec == TRUE ~ "Exotic",
            Native_Quebec == TRUE ~ "Native",
            TRUE ~ ""
          )
        )
    })

    # Create filtered ambiguous data table
    filtered_ambiguous <- shiny::reactive({
      station <- selected_station()
      # Filter ambiguous groups and split species
      ambiguous_data |>
        dplyr::filter(station == station_id) |>
        dplyr::mutate(group_id = paste0("Group ", dplyr::row_number())) |>
        tidyr::separate_longer_delim(Species, delim = " : ") |>
        dplyr::mutate(
          Species = trimws(Species),
          parts = strsplit(Species, "_")
        ) |>
        dplyr::mutate(
          Group = sapply(parts, function(x) if(length(x) >= 1) x[1] else NA),
          Genus = sapply(parts, function(x) if(length(x) >= 2) x[2] else NA),
          Species = sapply(parts, function(x) if(length(x) >= 3) x[3] else NA)
        ) |>
        dplyr::select(-parts) |>
        dplyr::mutate(
          scientific_name = paste(Genus, Species)
        ) |>
        dplyr::left_join(
          species_info |>
            dplyr::select(species, English, French, gbif_url, col_link, itis_url,
                         Native_Quebec, Exotic_Quebec, img),
          by = c("scientific_name" = "species")
        ) |>
        dplyr::mutate(
          Status = dplyr::case_when(
            Exotic_Quebec == TRUE ~ "Exotic",
            Native_Quebec == TRUE ~ "Native",
            TRUE ~ ""
          )
        )
    })

    # Render reactable
    output$species_table <- reactable::renderReactable({
      reactable::reactable(
        filtered_data() |>
          dplyr::select(img, Group, Genus, Species, English, French, gbif_url, col_link, itis_url, Status, n_reads) |>
          dplyr::arrange(dplyr::desc(n_reads)),
        searchable = TRUE,
        pagination = TRUE,
        defaultPageSize = 15,
        highlight = TRUE,
        bordered = TRUE,
        striped = FALSE,
        compact = TRUE,
        columns = list(
          img = reactable::colDef(
            name = "",
            minWidth = 120,
            cell = function(value) {
              if (!is.na(value)) {
                # Convert data/img/filename.jpg to img/filename.jpg for Shiny resource path
                img_path <- sub("^data/", "", value)
                htmltools::tags$img(src = img_path, width = "100%", style = "border-radius: 4px;")
              } else {
                # Show question mark icon for unknown species
                htmltools::tags$div(
                  style = "height: 80px; width: 100%; display: flex; align-items: center; justify-content: center; background-color: #f0f0f0; border-radius: 4px; color: #999; font-size: 32px;",
                  "?"
                )
              }
            }
          ),
          Group = reactable::colDef(
            show = FALSE
          ),
          Genus = reactable::colDef(
            show = FALSE
          ),
          Species = reactable::colDef(
            show = FALSE
          ),
          English = reactable::colDef(
            name = "Species",
            minWidth = 250,
            cell = function(value, index) {
              data <- filtered_data() |> dplyr::arrange(dplyr::desc(n_reads))
              family <- data$Group[index]
              genus <- data$Genus[index]
              species <- data$Species[index]
              french <- data$French[index]
              gbif <- data$gbif_url[index]
              col <- data$col_link[index]
              itis <- data$itis_url[index]

              # Build common names line
              common_names <- if (!is.na(value) && !is.na(french)) {
                paste0(value, " | ", french)
              } else if (!is.na(value)) {
                value
              } else if (!is.na(french)) {
                french
              } else {
                ""
              }

              # Build links
              links <- htmltools::tagList()
              if (!is.na(gbif)) {
                links <- htmltools::tagList(
                  links,
                  htmltools::tags$a(
                    href = gbif,
                    target = "_blank",
                    style = "color: #0066CC; margin-right: 10px;",
                    "Distribution"
                  )
                )
              }
              if (!is.na(col)) {
                links <- htmltools::tagList(
                  links,
                  htmltools::tags$a(
                    href = col,
                    target = "_blank",
                    style = "color: #0066CC; margin-right: 10px;",
                    "Encyclopedia"
                  )
                )
              }
              if (!is.na(itis)) {
                links <- htmltools::tagList(
                  links,
                  htmltools::tags$a(
                    href = itis,
                    target = "_blank",
                    style = "color: #0066CC;",
                    "Taxonomy"
                  )
                )
              }

              htmltools::tagList(
                htmltools::tags$div(
                  style = "font-weight: bold;",
                  family
                ),
                if (common_names != "") htmltools::tags$div(
                  style = "margin-top: 2px;",
                  common_names
                ),
                htmltools::tags$div(
                  style = "font-style: italic; font-size: 0.9em; color: #666; margin-top: 2px;",
                  paste(genus, species)
                ),
                if (length(links) > 0) {
                  htmltools::tags$div(
                    style = "margin-top: 4px; font-size: 0.85em;",
                    links
                  )
                }
              )
            }
          ),
          French = reactable::colDef(show = FALSE),
          gbif_url = reactable::colDef(show = FALSE),
          col_link = reactable::colDef(show = FALSE),
          itis_url = reactable::colDef(show = FALSE),
          Status = reactable::colDef(
            name = "Status",
            minWidth = 100,
            cell = function(value) {
              if (value == "Exotic") {
                htmltools::tags$span(
                  "\u26a0 Exotic"
                )
              } else if (value == "Native") {
                htmltools::tags$span(
                  "\u2713 Native"
                )
              } else {
                ""
              }
            }
          ),
          n_reads = reactable::colDef(
            name = "Reads",
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
        ),
        defaultColDef = reactable::colDef(
          headerStyle = list(fontWeight = 600)
        )
      )
    })

    # Render ambiguous groups table
    output$ambiguous_table <- reactable::renderReactable({
      data_sorted <- filtered_ambiguous() |>
        dplyr::select(img, group_id, Genus, Species, English, French, gbif_url, col_link, itis_url, Status, n_reads) |>
        dplyr::arrange(dplyr::desc(n_reads), group_id)

      reactable::reactable(
        data_sorted,
        groupBy = "group_id",
        defaultExpanded = TRUE,
        searchable = TRUE,
        pagination = TRUE,
        defaultPageSize = 15,
        highlight = TRUE,
        bordered = TRUE,
        striped = FALSE,
        compact = TRUE,
        columns = list(
          img = reactable::colDef(
            name = "",
            minWidth = 120,
            cell = function(value) {
              if (!is.na(value)) {
                # Convert data/img/filename.jpg to img/filename.jpg for Shiny resource path
                img_path <- sub("^data/", "", value)
                htmltools::tags$img(src = img_path, width = "100%", style = "border-radius: 4px;")
              } else {
                # Show question mark icon for unknown species
                htmltools::tags$div(
                  style = "height: 80px; width: 100%; display: flex; align-items: center; justify-content: center; background-color: #f0f0f0; border-radius: 4px; color: #999; font-size: 32px;",
                  "?"
                )
              }
            }
          ),
          group_id = reactable::colDef(
            name = "Group",
            minWidth = 150,
            aggregate = "count"
          ),
          Genus = reactable::colDef(
            show = FALSE
          ),
          Species = reactable::colDef(
            show = FALSE
          ),
          English = reactable::colDef(
            name = "Species",
            minWidth = 250,
            cell = function(value, index) {
              genus <- data_sorted$Genus[index]
              species <- data_sorted$Species[index]
              french <- data_sorted$French[index]
              gbif <- data_sorted$gbif_url[index]
              col <- data_sorted$col_link[index]
              itis <- data_sorted$itis_url[index]

              # Build common names line
              common_names <- if (!is.na(value) && !is.na(french)) {
                paste0(value, " | ", french)
              } else if (!is.na(value)) {
                value
              } else if (!is.na(french)) {
                french
              } else {
                ""
              }

              # Build links
              links <- htmltools::tagList()
              if (!is.na(gbif)) {
                links <- htmltools::tagList(
                  links,
                  htmltools::tags$a(
                    href = gbif,
                    target = "_blank",
                    style = "color: #0066CC; margin-right: 10px;",
                    "Distribution"
                  )
                )
              }
              if (!is.na(col)) {
                links <- htmltools::tagList(
                  links,
                  htmltools::tags$a(
                    href = col,
                    target = "_blank",
                    style = "color: #0066CC; margin-right: 10px;",
                    "Encyclopedia"
                  )
                )
              }
              if (!is.na(itis)) {
                links <- htmltools::tagList(
                  links,
                  htmltools::tags$a(
                    href = itis,
                    target = "_blank",
                    style = "color: #0066CC;",
                    "Taxonomy"
                  )
                )
              }

              htmltools::tagList(
                if (common_names != "") htmltools::tags$div(
                  style = "margin-top: 2px;",
                  common_names
                ),
                htmltools::tags$div(
                  style = "font-style: italic; font-size: 0.9em; color: #666; margin-top: 2px;",
                  paste(genus, species)
                ),
                if (length(links) > 0) {
                  htmltools::tags$div(
                    style = "margin-top: 4px; font-size: 0.85em;",
                    links
                  )
                }
              )
            }
          ),
          French = reactable::colDef(show = FALSE),
          gbif_url = reactable::colDef(show = FALSE),
          col_link = reactable::colDef(show = FALSE),
          itis_url = reactable::colDef(show = FALSE),
          Status = reactable::colDef(
            name = "Status",
            minWidth = 100,
            cell = function(value) {
              if (value == "Exotic") {
                htmltools::tags$span(
                  "\u26a0 Exotic"
                )
              } else if (value == "Native") {
                htmltools::tags$span(
                  "\u2713 Native"
                )
              } else {
                ""
              }
            }
          ),
          n_reads = reactable::colDef(
            name = "Reads",
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
        ),
        defaultColDef = reactable::colDef(
          headerStyle = list(fontWeight = 600)
        )
      )
    })

  })
}

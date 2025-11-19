# Species Table Module UI
speciesTableUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_card_tab(
    title = shiny::uiOutput(ns("card_title")),
    height = 600,
    full_screen = TRUE,
    bslib::nav_panel(
      title = htmltools::tagList(
        "Detected ",
        bslib::tooltip(
          bsicons::bs_icon("info-circle", size = "0.8em"),
          "Species with clear taxonomic assignments based on sequence matches."
        )
      ),
      reactable::reactableOutput(ns("species_table"))
    ),
    bslib::nav_panel(
      title = htmltools::tagList(
        "Ambiguous ",
        bslib::tooltip(
          bsicons::bs_icon("info-circle", size = "0.8em"),
          "Groups where sequences match multiple species equally well, indicating potential presence of any species within the group."
        )
      ),
      reactable::reactableOutput(ns("ambiguous_table"))
    )
  )
}

# Species Table Module Server
speciesTableServer <- function(id, species_data, ambiguous_data, selected_station) {
  shiny::moduleServer(id, function(input, output, session) {

    # Load species info
    species_info <- utils::read.csv(system.file("extdata", "species_info.csv", package = "metabarMap"), stringsAsFactors = FALSE)

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

    # Show detection status modal
    shiny::observeEvent(input$show_detection_modal, {
      shiny::showModal(
        shiny::modalDialog(
          title = htmltools::tags$div(
            style = "font-weight: bold;",
            "Detection Status"
          ),
          htmltools::tags$p("Species detections are classified into three confidence levels based on the number of DNA sequence reads:"),
          htmltools::tags$table(
            style = "width: 100%; border-collapse: collapse; margin: 1rem 0;",
            htmltools::tags$thead(
              htmltools::tags$tr(
                htmltools::tags$th(style = "border: 1px solid #ddd; padding: 8px; background-color: #f2f2f2;", "Detection status"),
                htmltools::tags$th(style = "border: 1px solid #ddd; padding: 8px; background-color: #f2f2f2;", "Read Count"),
                htmltools::tags$th(style = "border: 1px solid #ddd; padding: 8px; background-color: #f2f2f2;", "Interpretation")
              )
            ),
            htmltools::tags$tbody(
              htmltools::tags$tr(
                htmltools::tags$td(style = "border: 1px solid #ddd; padding: 8px;", htmltools::tags$strong("Confident")),
                htmltools::tags$td(style = "border: 1px solid #ddd; padding: 8px;", "> 100 reads"),
                htmltools::tags$td(style = "border: 1px solid #ddd; padding: 8px;", "High confidence detection with strong DNA signal")
              ),
              htmltools::tags$tr(
                htmltools::tags$td(style = "border: 1px solid #ddd; padding: 8px;", htmltools::tags$strong("Probable")),
                htmltools::tags$td(style = "border: 1px solid #ddd; padding: 8px;", "10-100 reads"),
                htmltools::tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Moderate confidence detection")
              ),
              htmltools::tags$tr(
                htmltools::tags$td(style = "border: 1px solid #ddd; padding: 8px;", htmltools::tags$strong("Uncertain")),
                htmltools::tags$td(style = "border: 1px solid #ddd; padding: 8px;", "< 10 reads"),
                htmltools::tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Low confidence detection, may require additional verification")
              )
            )
          ),
          htmltools::tags$p(
            style = "margin-top: 1rem; font-style: italic;",
            htmltools::tags$strong("Note:"), " Read counts reflect DNA concentrations detected in the sample, but are not directly proportional to organism abundance due to variations in the laboratory process and species autoecology."
          ),
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        )
      )
    })

    # Create filtered species table
    filtered_data <- shiny::reactive({
      station <- selected_station()
      # Filter and prepare data
      species_data |>
        dplyr::filter(station_id == station) |>
        dplyr::mutate(scientific_name = paste(Genus, Species)) |>
        dplyr::left_join(
          species_info |>
            dplyr::select(species, English, French, gbif_url, col_link, itis_url,
                         Native_Quebec, Exotic_Quebec, status_ca, status_qc, img),
          by = c("scientific_name" = "species")
        ) |>
        dplyr::mutate(
          # Translate conservation status to English
          status_ca_en = dplyr::case_when(
            is.na(status_ca) ~ NA_character_,
            status_ca == "En voie de disparition" ~ "Endangered",
            status_ca == "Menac\u00e9e" ~ "Threatened",
            status_ca == "Pr\u00e9occupante" ~ "Special Concern",
            status_ca == "Disparue du pays" ~ "Extirpated",
            TRUE ~ status_ca
          ),
          status_qc_en = dplyr::case_when(
            is.na(status_qc) ~ NA_character_,
            status_qc == "Menac\u00e9e" ~ "Threatened",
            status_qc == "Vuln\u00e9rable" ~ "Vulnerable",
            status_qc == "Susceptible" ~ "Likely to be designated",
            TRUE ~ status_qc
          ),
          # Create combined Status column
          Status = dplyr::case_when(
            Exotic_Quebec == TRUE ~ "Exotic",
            Native_Quebec == TRUE ~ "Native",
            TRUE ~ ""
          ),
          detection_status = dplyr::case_when(
            n_reads > 100 ~ "Confident",
            n_reads > 10 ~ "Probable",
            TRUE ~ "Uncertain"
          )
        )
    })

    # Create filtered ambiguous data table
    filtered_ambiguous <- shiny::reactive({
      station <- selected_station()
      # Filter ambiguous groups and split species
      ambiguous_data |>
        dplyr::filter(station_id == station) |>
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
                         Native_Quebec, Exotic_Quebec, status_ca, status_qc, img),
          by = c("scientific_name" = "species")
        ) |>
        dplyr::mutate(
          # Translate conservation status to English
          status_ca_en = dplyr::case_when(
            is.na(status_ca) ~ NA_character_,
            status_ca == "En voie de disparition" ~ "Endangered",
            status_ca == "Menac\u00e9e" ~ "Threatened",
            status_ca == "Pr\u00e9occupante" ~ "Special Concern",
            status_ca == "Disparue du pays" ~ "Extirpated",
            TRUE ~ status_ca
          ),
          status_qc_en = dplyr::case_when(
            is.na(status_qc) ~ NA_character_,
            status_qc == "Menac\u00e9e" ~ "Threatened",
            status_qc == "Vuln\u00e9rable" ~ "Vulnerable",
            status_qc == "Susceptible" ~ "Likely to be designated",
            TRUE ~ status_qc
          ),
          # Create combined Status column
          Status = dplyr::case_when(
            Exotic_Quebec == TRUE ~ "Exotic",
            Native_Quebec == TRUE ~ "Native",
            TRUE ~ ""
          ),
          detection_status = dplyr::case_when(
            n_reads > 100 ~ "Confident",
            n_reads > 10 ~ "Probable",
            TRUE ~ "Uncertain"
          )
        )
    })

    # Render reactable
    output$species_table <- reactable::renderReactable({
      data_sorted <- filtered_data() |>
        dplyr::select(img, Group, Genus, Species, English, French, gbif_url, col_link, itis_url, Status, status_qc_en, status_ca_en, n_reads, detection_status) |>
        dplyr::arrange(dplyr::desc(n_reads))

      reactable::reactable(
        data_sorted,
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
            minWidth = 150,
            html = TRUE,
            cell = function(value, index) {
              status_ca <- data_sorted$status_ca_en[index]
              status_qc <- data_sorted$status_qc_en[index]

              parts <- list()

              # Add Native/Exotic status
              if (value == "Exotic") {
                parts <- c(parts, list(htmltools::tags$div("\u26a0 Exotic")))
              } else if (value == "Native") {
                parts <- c(parts, list(htmltools::tags$div("\u2713 Native")))
              }

              # Add CA conservation status
              if (!is.na(status_ca)) {
                parts <- c(parts, list(
                  htmltools::tags$div(
                    style = "color: #d9534f; margin-top: 4px;",
                    paste0("In Canada: ", status_ca)
                  )
                ))
              }

              # Add QC conservation status
              if (!is.na(status_qc)) {
                parts <- c(parts, list(
                  htmltools::tags$div(
                    style = "color: #d9534f; margin-top: 4px;",
                    paste0("In Quebec: ", status_qc)
                  )
                ))
              }

              if (length(parts) > 0) {
                htmltools::tagList(parts)
              } else {
                ""
              }
            }
          ),
          status_qc_en = reactable::colDef(show = FALSE),
          status_ca_en = reactable::colDef(show = FALSE),
          n_reads = reactable::colDef(
            show = FALSE
          ),
          detection_status = reactable::colDef(
            name = "Detection status",
            header = function(value) {
              htmltools::HTML(
                paste0(
                  '<span>Detection status ',
                  '<a href="#" onclick="Shiny.setInputValue(\'species_table_module-show_detection_modal\', Math.random()); return false;" ',
                  'style="color: #0066CC; cursor: pointer; text-decoration: none; margin-left: 5px;">',
                  '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" width="14" height="14" fill="currentColor">',
                  '<circle cx="8" cy="8" r="7" stroke="currentColor" stroke-width="1" fill="none"/>',
                  '<text x="8" y="12" font-size="12" text-anchor="middle" fill="currentColor" font-weight="bold">i</text>',
                  '</svg>',
                  '</a></span>'
                )
              )
            },
            html = TRUE,
            minWidth = 150,
            align = "left",
            cell = function(value, index) {
              data_sorted <- filtered_data() |> dplyr::arrange(dplyr::desc(n_reads))
              reads <- data_sorted$n_reads[index]
              formatted_reads <- format(reads, big.mark = ",")

              color <- if (value == "Confident") {
                "#00A000"
              } else if (value == "Probable") {
                "#FFA500"
              } else {
                "#999999"
              }

              htmltools::tagList(
                htmltools::tags$div(
                  style = paste0("color: ", color, "; font-weight: bold;"),
                  value
                ),
                htmltools::tags$div(
                  style = "color: inherit; font-weight: normal; font-size: 0.9em; margin-top: 2px;",
                  paste0(format(reads, big.mark = " "), " reads")
                )
              )
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
        dplyr::select(img, group_id, Genus, Species, English, French, gbif_url, col_link, itis_url, Status, status_qc_en, status_ca_en, n_reads, detection_status) |>
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
                htmltools::tags$img(src = value, width = "100%", style = "border-radius: 4px;")
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
            minWidth = 150,
            html = TRUE,
            cell = function(value, index) {
              status_ca <- data_sorted$status_ca_en[index]
              status_qc <- data_sorted$status_qc_en[index]

              parts <- list()

              # Add Native/Exotic status
              if (value == "Exotic") {
                parts <- c(parts, list(htmltools::tags$div("\u26a0 Exotic")))
              } else if (value == "Native") {
                parts <- c(parts, list(htmltools::tags$div("\u2713 Native")))
              }

              # Add CA conservation status
              if (!is.na(status_ca)) {
                parts <- c(parts, list(
                  htmltools::tags$div(
                    style = "color: #d9534f; margin-top: 4px;",
                    paste0("In Canada: ", status_ca)
                  )
                ))
              }

              # Add QC conservation status
              if (!is.na(status_qc)) {
                parts <- c(parts, list(
                  htmltools::tags$div(
                    style = "color: #d9534f; margin-top: 4px;",
                    paste0("In Quebec: ", status_qc)
                  )
                ))
              }

              if (length(parts) > 0) {
                htmltools::tagList(parts)
              } else {
                ""
              }
            }
          ),
          status_qc_en = reactable::colDef(show = FALSE),
          status_ca_en = reactable::colDef(show = FALSE),
          n_reads = reactable::colDef(
            show = FALSE
          ),
          detection_status = reactable::colDef(
            name = "Detection status",
            header = function(value) {
              htmltools::HTML(
                paste0(
                  '<span>Detection status ',
                  '<a href="#" onclick="Shiny.setInputValue(\'species_table_module-show_detection_modal\', Math.random()); return false;" ',
                  'style="color: #0066CC; cursor: pointer; text-decoration: none; margin-left: 5px;">',
                  '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" width="14" height="14" fill="currentColor">',
                  '<circle cx="8" cy="8" r="7" stroke="currentColor" stroke-width="1" fill="none"/>',
                  '<text x="8" y="12" font-size="12" text-anchor="middle" fill="currentColor" font-weight="bold">i</text>',
                  '</svg>',
                  '</a></span>'
                )
              )
            },
            html = TRUE,
            minWidth = 150,
            align = "left",
            cell = function(value, index) {
              reads <- data_sorted$n_reads[index]
              formatted_reads <- format(reads, big.mark = ",")

              color <- if (value == "Confident") {
                "#00A000"
              } else if (value == "Probable") {
                "#FFA500"
              } else {
                "#999999"
              }

              htmltools::tagList(
                htmltools::tags$div(
                  style = paste0("color: ", color, "; font-weight: bold;"),
                  value
                ),
                htmltools::tags$div(
                  style = "color: inherit; font-weight: normal; font-size: 0.9em; margin-top: 2px;",
                  paste0(format(reads, big.mark = " "), " reads")
                )
              )
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

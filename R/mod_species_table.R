# Species Table Module UI
speciesTableUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_card_tab(
    title = shiny::uiOutput(ns("card_title")),
    height = 600,
    full_screen = TRUE,
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = htmltools::tagList(
        "Détectées ",
        bslib::tooltip(
          bsicons::bs_icon("info-circle", size = "0.8em"),
          "Espèces avec des assignations taxonomiques claires basées sur les correspondances de séquences."
        )
      ),
      reactable::reactableOutput(ns("species_table"))
    ),
    )
}

# Species Table Module Server
speciesTableServer <- function(id, species_data, species_info, selected_station) {
  shiny::moduleServer(id, function(input, output, session) {

    # Dynamic card title
    output$card_title <- shiny::renderUI({
      station <- selected_station()
      if (!is.null(station)) {
        shiny::tags$h5(
          shiny::span(station, style = "color: #0066CC;")
        )
      } else {
        shiny::tags$h5("Espèces")
      }
    })

    # Show detection status modal
    shiny::observeEvent(input$show_detection_modal, {
      shiny::showModal(
        shiny::modalDialog(
          title = htmltools::tags$div(
            style = "font-weight: bold;",
            "Statut de détection"
          ),
          htmltools::tags$p("Les détections d'espèces sont classées en trois niveaux de confiance selon le nombre de lectures de séquences ADN :"),
          htmltools::tags$table(
            style = "width: 100%; border-collapse: collapse; margin: 1rem 0;",
            htmltools::tags$thead(
              htmltools::tags$tr(
                htmltools::tags$th(style = "border: 1px solid #ddd; padding: 8px; background-color: #f2f2f2;", "Statut de détection"),
                htmltools::tags$th(style = "border: 1px solid #ddd; padding: 8px; background-color: #f2f2f2;", "Nombre de lectures"),
                htmltools::tags$th(style = "border: 1px solid #ddd; padding: 8px; background-color: #f2f2f2;", "Interprétation")
              )
            ),
            htmltools::tags$tbody(
              htmltools::tags$tr(
                htmltools::tags$td(style = "border: 1px solid #ddd; padding: 8px;", htmltools::tags$strong("Confiant")),
                htmltools::tags$td(style = "border: 1px solid #ddd; padding: 8px;", "> 100 lectures"),
                htmltools::tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Détection hautement fiable avec un signal ADN fort")
              ),
              htmltools::tags$tr(
                htmltools::tags$td(style = "border: 1px solid #ddd; padding: 8px;", htmltools::tags$strong("Probable")),
                htmltools::tags$td(style = "border: 1px solid #ddd; padding: 8px;", "10-100 lectures"),
                htmltools::tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Détection à confiance modérée")
              ),
              htmltools::tags$tr(
                htmltools::tags$td(style = "border: 1px solid #ddd; padding: 8px;", htmltools::tags$strong("Incertain")),
                htmltools::tags$td(style = "border: 1px solid #ddd; padding: 8px;", "< 10 lectures"),
                htmltools::tags$td(style = "border: 1px solid #ddd; padding: 8px;", "Détection peu fiable, une vérification supplémentaire peut être nécessaire")
              )
            )
          ),
          htmltools::tags$p(
            style = "margin-top: 1rem; font-style: italic;",
            htmltools::tags$strong("Note :"), " Le nombre de lectures reflète les concentrations d'ADN détectées dans l'échantillon, mais n'est pas directement proportionnel à l'abondance des organismes en raison des variations du processus de laboratoire et de l'autoécologie des espèces."
          ),
          easyClose = TRUE,
          footer = shiny::modalButton("Fermer")
        )
      )
    })

    # Create filtered species table
    filtered_data <- shiny::reactive({
      station <- selected_station()
      # Filter and prepare data
      species_data |>
        dplyr::filter(station_id == station, n_reads > 0) |>
        dplyr::mutate(
          scientific_name = paste(Genus, Species),
          Group = tools::toTitleCase(tolower(Group))
        ) |>
        dplyr::left_join(
          species_info |>
            dplyr::select(species, English, French, col_url, itis_url,
                         Native_Quebec, Exotic_Quebec, status_ca, status_qc, img),
          by = c("scientific_name" = "species")
        ) |>
        dplyr::mutate(
          status_ca_en = as.character(status_ca),
          status_qc_en = as.character(status_qc),
          Status = dplyr::case_when(
            Exotic_Quebec == TRUE ~ "Exotic",
            Native_Quebec == TRUE ~ "Native",
            TRUE ~ ""
          ),
          detection_status = dplyr::if_else(
            !is.na(qpcr_detection_status), qpcr_detection_status, "Détecté"
          )
        )
    })

    # Render reactable
    output$species_table <- reactable::renderReactable({
      data_sorted <- filtered_data() |>
        dplyr::select(img, Group, Genus, Species, English, French, col_url, itis_url, DetectionMethod, detection_status, Status, status_qc_en, status_ca_en, n_reads) |>
        dplyr::arrange(Group, Genus, Species)

      reactable::reactable(
        data_sorted,
        searchable = TRUE,
        pagination = TRUE,
        defaultPageSize = 50,
        highlight = TRUE,
        bordered = TRUE,
        striped = FALSE,
        compact = TRUE,
        style = list(fontSize = "0.85em"),
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
            name = "Espèces",
            minWidth = 250,
            cell = function(value, index) {
              family <- data_sorted$Group[index]
              genus <- data_sorted$Genus[index]
              species <- data_sorted$Species[index]
              french <- data_sorted$French[index]
              col <- data_sorted$col_url[index]
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
              if (!is.na(col)) {
                links <- htmltools::tagList(
                  links,
                  htmltools::tags$a(
                    href = col,
                    target = "_blank",
                    style = "color: #0066CC; margin-right: 10px;",
                    "Encyclopédie"
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
                    "Taxonomie"
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
          col_url = reactable::colDef(show = FALSE),
          itis_url = reactable::colDef(show = FALSE),
          Status = reactable::colDef(
            name = "Statut",
            minWidth = 150,
            html = TRUE,
            cell = function(value, index) {
              status_ca <- data_sorted$status_ca_en[index]
              status_qc <- data_sorted$status_qc_en[index]

              parts <- list()

              # Add CA conservation status
              if (!is.na(status_ca)) {
                parts <- c(parts, list(
                  htmltools::tags$div(
                    style = "color: #d9534f; margin-top: 4px;",
                    paste0("Au Canada : ", status_ca)
                  )
                ))
              }

              # Add QC conservation status
              if (!is.na(status_qc)) {
                parts <- c(parts, list(
                  htmltools::tags$div(
                    style = "color: #d9534f; margin-top: 4px;",
                    paste0("Au Québec : ", status_qc)
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
            name = "Statut de détection",
            header = function(value) {
              htmltools::HTML(
                paste0(
                  '<span>Statut de détection ',
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
              method <- data_sorted$DetectionMethod[index]
              color <- if (!is.na(method) && method == "qPCR") "#0066CC" else "#00A000"

              htmltools::tags$div(
                style = paste0("color: ", color, "; font-weight: bold;"),
                value
              )
            }
          ),
          DetectionMethod = reactable::colDef(
            name = "Méthode",
            minWidth = 100,
            align = "left"
          )
        ),
        defaultColDef = reactable::colDef(
          headerStyle = list(fontWeight = 600)
        )
      )
    })


  })
}

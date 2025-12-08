#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = shiny::tags$div(
        shiny::tags$div(
          shiny::tags$img(
            src = get_golem_config("project_logo"),
            height = "100px",
            style = "vertical-align: middle"
          ),
          shiny::tags$h3(
            get_golem_config("project_name"),
            style = "font-weight: bold; margin: 0; margin-bottom:10px;"
          )
        ),
        shiny::tags$div(
          style = "display: flex; gap: 10px;",
          shiny::actionButton(
            "toggle_methods",
            "Methods",
            icon = shiny::icon("circle-info"),
            class = "btn-dark"
          ),
          shiny::downloadButton(
            "download_all_data",
            "Download data",
            icon = shiny::icon("download"),
            class = "btn-primary"
          )
        )
      ),
      theme = bslib::bs_theme(
        preset = "shiny",
        base_font = bslib::font_google("Inter"),
        code_font = bslib::font_google("JetBrains Mono")
      ),
      fillable = TRUE,

      bslib::nav_panel(
        title = NULL,
        icon = NULL,

        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            id = "methods_sidebar",
            open = FALSE,
            position = "right",
            width = "500px",
            fillable = FALSE,
            fill = FALSE,
            gap = "10px",
            shiny::uiOutput("methods_content")
          ),

          bslib::layout_columns(
            col_widths = c(6, 6),

            # Map panel
            bslib::card(
              bslib::card_header(
                shiny::tags$h5(
                  "Sampling stations"
                )
              ),
              bslib::card_body(
                class = "p-0",
                mapUI("map_module")
              )
            ),

            # Species table panel
            speciesTableUI("species_table_module")
          )
        )
      ),

      bslib::nav_spacer(),

      # Partner logos
      bslib::nav_item(
        shiny::uiOutput("partner_logos")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "metabarMap"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

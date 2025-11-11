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
      title = "MetabarMap",
      theme = bslib::bs_theme(
        preset = "shiny",
        base_font = bslib::font_google("Inter"),
        code_font = bslib::font_google("JetBrains Mono")
      ),
      fillable = TRUE,

      bslib::nav_panel(
        title = "Explore",
        icon = shiny::icon("map-location-dot"),

        bslib::layout_columns(
          col_widths = c(7, 5),

          # Map panel
          bslib::card(
            bslib::card_header(h4("Sampling stations")),
            bslib::card_body(
              mapUI("map_module")
            )
          ),

          # Species table panel
          bslib::card(
            bslib::card_header(
              h4(shiny::uiOutput("species_table_module-card_header"))
            ),
            bslib::card_body(
              speciesTableUI("species_table_module")
            )
          )
        )
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

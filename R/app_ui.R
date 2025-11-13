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
        style = "display: flex; align-items: center; gap: 15px;",
        shiny::tags$img(
          src = get_golem_config("project_logo"),
          height = "80px",
          style = "vertical-align: middle;"
        ),
        shiny::tags$span(
          get_golem_config("project_name"),
          style = "font-weight: 500;"
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
      ),

      bslib::nav_spacer(),

      bslib::nav_item(
        shiny::tags$div(
          style = "display: flex; align-items: center; height: 100%;",
          shiny::downloadButton(
            "download_all_data",
            "Download data",
            icon = shiny::icon("download"),
            class = "btn-outline-primary"
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

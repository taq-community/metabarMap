#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "metabarMap")
}


#' Read App Config
#'
#' Reads a value from the `default` section of `inst/golem-config.yml`.
#'
#' @param value Value to retrieve from the config file.
#'
#' @noRd
get_golem_config <- function(value) {
  yaml::read_yaml(app_sys("golem-config.yml"))$default[[value]]
}

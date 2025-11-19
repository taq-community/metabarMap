#' Global variables
#'
#' Suppress R CMD check notes about global variables used in dplyr/tidyr chains
#' @noRd
utils::globalVariables(c(
  # Column names from data frames
  "English",
  "Exotic_Quebec",
  "French",
  "Genus",
  "Group",
  "Native_Quebec",
  "Species",
  "Status",
  "TaxonName",
  "col_link",
  "detection_status",
  "gbif_url",
  "group_id",
  "itis_url",
  "n_reads",
  "parts",
  "species",
  "station_id",
  "status_ca",
  "status_ca_en",
  "status_qc",
  "status_qc_en"
))

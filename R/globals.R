#' Global variables
#'
#' Suppress R CMD check notes about global variables used in dplyr/tidyr chains
#' @noRd
utils::globalVariables(c(
  # Column names from data frames
  "DetectionMethod",
  "English",
  "Exotic_Quebec",
  "French",
  "Genus",
  "Group",
  "Native_Quebec",
  "Species",
  "Status",
  "TaxonName",
  "col_url",
  "detection_order",
  "detection_status",
  "qpcr_detection_status",
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

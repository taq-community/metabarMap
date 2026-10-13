# Inputs (not versioned): inst/extdata/species_table.csv, inst/extdata/species_info.csv
library(dplyr)
library(tidyr)

classify_detection_metabar <- function(n) {
    dplyr::case_when(
        n == 0 ~ "Non-détecté",
        n <= 10 ~ "Faible",
        TRUE ~ "Détecté"
    )
}

# ── Load raw data ─────────────────────────────────────────────────────────────

species_table <- read.csv("inst/extdata/species_table.csv", check.names = FALSE) |>
  filter(Group != "Total")

species_info <- read.csv("inst/extdata/species_info.csv", stringsAsFactors = FALSE)

# Station columns: everything after the fixed metadata columns
station_cols <- setdiff(
  names(species_table),
  c("Group", "Genus", "Species", "TaxonName", "DetectionMethod", "Total")
)

# ── species_data: detected species (non-ambiguous, non-qPCR) ─────────────────

species_data_all <- species_table |>
    filter(
        TaxonName != "sample", Group != "MultipleHits", Group != "zMultiple",
        DetectionMethod != "qPCR"
    ) |>
    select(Group, Genus, Species, DetectionMethod, all_of(station_cols)) |>
    pivot_longer(all_of(station_cols), names_to = "station_id", values_to = "n_reads") |>
    filter(!is.na(n_reads), n_reads > 0) |>
    mutate(scientific_name = paste(Genus, Species)) |>
    left_join(species_info, by = c("scientific_name" = "species")) |>
    select(-scientific_name) |>
    mutate(detection_status = classify_detection_metabar(n_reads)) |>
    select(-n_reads) |>
    pivot_wider(names_from = station_id, values_from = detection_status)

# Split into a named list by detection method
species_data <- split(species_data_all, species_data_all$DetectionMethod)

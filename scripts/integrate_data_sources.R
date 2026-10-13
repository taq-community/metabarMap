# Inputs (not versioned): inst/extdata/{species_table_12s_rn,species_table_COI_rn,COI_12S_RN,qPCR}.csv
# Output: inst/extdata/species_table.csv
library(dplyr)
library(readr)
library(tidyr)
library(taxize)

# ── Helpers ──────────────────────────────────────────────────────────────────
species_table_12S <- read_delim("inst/extdata/species_table_12s_rn.csv", delim = ";", show_col_types = FALSE)
species_table_COI <- read_delim("inst/extdata/species_table_COI_rn.csv", delim = ";", show_col_types = FALSE)

# Manual overrides for truncated/inconsistent TaxonName keys in COI_12S_RN.csv.
# Each entry maps the key as it appears in that file to its DetectionMethod.
# - chordata_Catostomus: present in both 12S (Catostomidae_Catostomus_*) and COI
# - Luxilus cornutus: only Luxilus_zonatus exists in 12S (different species); flagged NA
# - Amnicola limosa: absent from both tables entirely; flagged NA
detection_overrides <- c(
  "arthropoda_Holopedium" = "COI",   # arthropoda_Holopedium_glacialis
  "arthropoda_Caenis"     = "COI",   # arthropoda_Caenis_youngi
  "chordata_Catostomus"   = "12S, COI",
  "Myotis"                = "COI",   # chordata_Myotis_albescens
  "Cottidae_Cottus"       = "12S",   # Cottidae_Cottus_perifretum / volki
  "Polyarthra"            = "COI",   # rotifera_Polyarthra_*
  "Luxilus cornutus"      = NA_character_,  # only Luxilus_zonatus in 12S — different species
  "Amnicola limosa"       = NA_character_   # absent from both tables
)

species_table <- read.csv2("inst/extdata/COI_12S_RN.csv", check.names = FALSE) |>
  mutate(
    DetectionMethod = case_when(
      Espece %in% names(detection_overrides) ~ detection_overrides[Espece],
      Espece %in% species_table_12S$TaxonName & Espece %in% species_table_COI$TaxonName ~ "12S, COI",
      Espece %in% species_table_12S$TaxonName ~ "12S",
      Espece %in% species_table_COI$TaxonName ~ "COI",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    Genus   = sub("^[^_]+_([^_]+)_.*$", "\\1", Espece),
    Species = sub("^[^_]+_[^_]+_(.+)$", "\\1", Espece)
  ) |>
  rename(TaxonName = "Espece", Group = "Embranchement")


# ── qPCR: pivot wide and align columns to species_table ───────────────────

qpcr_wide <- read_delim("inst/extdata/qPCR.csv", delim = ";", show_col_types = FALSE,
                         locale = locale(decimal_mark = ",")) |>
  select(espece, station, `copies/L`) |>
  pivot_wider(names_from = station, values_from = `copies/L`, values_fill = NA) |>
  mutate(
    Group           = "Crustacée",
    Genus           = "Bythotrephes",
    Species         = "longimanus",
    TaxonName       = "Bythotrephes_longimanus",
    DetectionMethod = "qPCR"
  ) |>
  select(-espece)

# ── Bind species_table + qPCR ─────────────────────────────────────────────
integrated <- bind_rows(species_table, qpcr_wide)

write_csv(integrated, "inst/extdata/species_table.csv")


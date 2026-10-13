library(dplyr)
library(sf)
library(tmap)

# ── Load data ─────────────────────────────────────────────────────────────────

source("scripts/generate_dfs.R")
localisations <- read.csv("inst/extdata/localisations.csv")

# ── Species richness per station per detection method ─────────────────────────

station_cols <- setdiff(names(species_data_all), c(
  "Group", "Genus", "Species", "DetectionMethod",
  "English", "French", "col_url", "itis_url",
  "Native_Quebec", "Exotic_Quebec", "status_ca", "status_qc",
  "detection_status"
))

richness <- species_data_all |>
  select(Genus, Species, DetectionMethod, all_of(station_cols)) |>
  pivot_longer(all_of(station_cols), names_to = "station", values_to = "detection") |>
  filter(!is.na(detection), detection != "Non-détecté") |>
  distinct(Genus, Species, DetectionMethod, station) |>
  count(DetectionMethod, station, name = "n_species")

map_data <- localisations |>
  left_join(richness, by = "station") |>
  filter(!is.na(DetectionMethod)) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326)

bbox_expanded <- st_bbox(
  c(xmin = -70.30, ymin = 47.86, xmax = -69.82, ymax = 48.32),
  crs = 4326
) |> st_as_sfc()

# ── 12S and COI maps ──────────────────────────────────────────────────────────

tmap_mode("plot")

map_12S <- tm_shape(bbox_expanded, is.master = TRUE) +
  tm_tiles("Esri.WorldImagery") +
  tm_shape(map_data |> filter(DetectionMethod == "12S")) +
  tm_bubbles(
    fill        = "n_species",
    fill.scale  = tm_scale_continuous(values = "YlOrRd"),
    fill.legend = tm_legend(title = "Nb espèces detectées", frame = FALSE, reverse = TRUE),
    size = 0.8
  ) +
  tm_labels(
      text = "station",
      avoid.overlap = TRUE,
      point.label = TRUE,
      size = 0.7,
      col = "black",
      bgcol = "white",
      bgcol_alpha = 0.6,
      xmod = 1.5,
      ymod = 0
  ) +
  tm_graticules(col = "white", lwd = 0.3, labels.size = 0.5) +
  tm_title("Petit-Sagunay\nRichesse spécifique - 12S")

map_COI <- tm_shape(bbox_expanded, is.master = TRUE) +
  tm_tiles("Esri.WorldImagery") +
  tm_shape(map_data |> filter(DetectionMethod == "COI")) +
  tm_bubbles(
    fill        = "n_species",
    fill.scale  = tm_scale_continuous(values = "YlOrRd"),
    fill.legend = tm_legend(title = "Nb espèces detectées", frame = FALSE, reverse = TRUE),
    size = 1
  ) +
  tm_labels(
      text = "station",
      avoid.overlap = TRUE, # drops labels that would overlap
      point.label = TRUE, # optimizes placement around points
      size = 0.7,
      col = "black",
      bgcol = "white",
      bgcol_alpha = 0.6,
      xmod = 1.5,
      ymod = 0
  ) +
  tm_graticules(col = "white", lwd = 0.3, labels.size = 0.5) +
  tm_title("Petit-Sagunay\nRichesse spécifique - COI")

# ── qPCR data: one map per species ───────────────────────────────────────────

qpcr_raw <- read.csv2("inst/extdata/qPCR.csv", stringsAsFactors = FALSE) |>
  rename(copies_L = "copies.L") |>
  mutate(
    copies_L  = as.numeric(gsub(",", ".", copies_L)),
    detection = trimws(detection),
    detection = factor(detection, levels = c("Non détecté", "Faible", "Détecté"))
  )

qpcr_sf <- localisations |>
  left_join(qpcr_raw, by = "station") |>
  filter(!is.na(espece)) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326)

map_qpcr <- lapply(unique(qpcr_sf$espece), function(sp) {
  tm_shape(bbox_expanded, is.master = TRUE) +
      tm_tiles("Esri.WorldImagery") +
      tm_shape(qpcr_sf |> filter(espece == sp)) +
      tm_bubbles(
          size = "copies_L",
          fill = "detection",
          fill.scale = tm_scale_categorical(
              values = c("Non détecté" = "#AAAAAA", "Faible" = "#f7c569", "Détecté" = "#00805e")
          ),
          fill.legend = tm_legend(
              title = "Détection", frame = FALSE,
              position = tm_pos_out("right", "center")
          ),
          size.scale = tm_scale_continuous(values.scale = 2),
          size.legend = tm_legend(
              title = "Copies/L", frame = FALSE,
              position = tm_pos_out("right", "center")
          )
      ) +
        tm_labels(
            text = "station",
            avoid.overlap = TRUE,
            point.label = TRUE,
            size = 0.7,
            col = "black",
            bgcol = "white",
            bgcol_alpha = 0.6,
            xmod = 1.5,
            ymod = 0
        ) +
    tm_graticules(col = "white", lwd = 0.3, labels.size = 0.5) +
    tm_title(sp)
})
names(map_qpcr) <- unique(qpcr_sf$espece)


# ── Versions sans labels ──────────────────────────────────────────────────────

map_12S_nolabel <- tm_shape(bbox_expanded, is.master = TRUE) +
  tm_tiles("Esri.WorldImagery") +
  tm_shape(map_data |> filter(DetectionMethod == "12S")) +
  tm_bubbles(
    fill        = "n_species",
    fill.scale  = tm_scale_continuous(values = "YlOrRd"),
    fill.legend = tm_legend(title = "Nb espèces detectées", frame = FALSE, reverse = TRUE),
    size = 0.8
  ) +
  tm_graticules(col = "white", lwd = 0.3, labels.size = 0.5) +
  tm_title("Petit-Sagunay\nRichesse spécifique - 12S")

map_COI_nolabel <- tm_shape(bbox_expanded, is.master = TRUE) +
  tm_tiles("Esri.WorldImagery") +
  tm_shape(map_data |> filter(DetectionMethod == "COI")) +
  tm_bubbles(
    fill        = "n_species",
    fill.scale  = tm_scale_continuous(values = "YlOrRd"),
    fill.legend = tm_legend(title = "Nb espèces detectées", frame = FALSE, reverse = TRUE),
    size = 1
  ) +
  tm_graticules(col = "white", lwd = 0.3, labels.size = 0.5) +
  tm_title("Petit-Sagunay\nRichesse spécifique - COI")

map_qpcr_nolabel <- lapply(unique(qpcr_sf$espece), function(sp) {
  tm_shape(bbox_expanded, is.master = TRUE) +
      tm_tiles("Esri.WorldImagery") +
      tm_shape(qpcr_sf |> filter(espece == sp)) +
      tm_bubbles(
          size = "copies_L",
          fill = "detection",
          fill.scale = tm_scale_categorical(
              values = c("Non détecté" = "#AAAAAA", "Faible" = "#f7c569", "Détecté" = "#00805e")
          ),
          fill.legend = tm_legend(
              title = "Détection", frame = FALSE,
              position = tm_pos_out("right", "center")
          ),
          size.scale = tm_scale_continuous(values.scale = 2),
          size.legend = tm_legend(
              title = "Copies/L", frame = FALSE,
              position = tm_pos_out("right", "center")
          )
      ) +
    tm_graticules(col = "white", lwd = 0.3, labels.size = 0.5) +
    tm_title(sp)
})
names(map_qpcr_nolabel) <- unique(qpcr_sf$espece)

# ── Save ──────────────────────────────────────────────────────────────────────

tmap_save(map_12S, "map_12S.png", width = 1800, height = 1200, dpi = 150)
tmap_save(map_COI, "map_COI.png", width = 1800, height = 1200, dpi = 150)

for (sp in names(map_qpcr)) {
  filename <- paste0("map_qpcr_", gsub(" ", "_", sp), ".png")
  tmap_save(map_qpcr[[sp]], filename, width = 1800, height = 1200, dpi = 150)
}

tmap_save(map_12S_nolabel, "map_12S_nolabel.png", width = 1800, height = 1200, dpi = 150)
tmap_save(map_COI_nolabel, "map_COI_nolabel.png", width = 1800, height = 1200, dpi = 150)

for (sp in names(map_qpcr_nolabel)) {
  filename <- paste0("map_qpcr_", gsub(" ", "_", sp), "_nolabel.png")
  tmap_save(map_qpcr_nolabel[[sp]], filename, width = 1800, height = 1200, dpi = 150)
}

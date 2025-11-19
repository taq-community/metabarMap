get_species_from_barque <- function(path = NULL) {
    read.csv(path) |>
        dplyr::select(Group, Genus, Species) |>
        tidyr::separate_longer_delim(Species, delim = " : ") |>
        dplyr::rowwise() |>
        dplyr::mutate(
            parts = list(if (Genus == "Hits") strsplit(Species, "_")[[1]] else c(Group, Genus, Species)),
            Group = parts[1],
            Genus = parts[2],
            Species = parts[3]
        ) |>
        dplyr::ungroup() |>
        dplyr::select(-parts) |>
        dplyr::filter(Group != "Total") |>
        dplyr::distinct()
}

sps <- get_species_from_barque("inst/extdata/species_table.csv") |>
    dplyr::reframe(sp = paste(Genus, Species)) |>
    dplyr::pull(sp)


require(WikipediR)
require(rvest)

getwikipic <- function(titles, res = 220, savedest = NA) {
    lapply(titles, function(ttl, ...) {
        d <- page_info("en", "wikipedia", page = ttl, clean_response = T)
        url <- d[[1]]$fullurl
        wikipage <- session(url)
        imginfo <- wikipage %>% html_nodes("tr:nth-child(2) img")
        img.url <- imginfo[1] %>% html_attr("src")
        img.url <- paste0("https:", img.url)

        if (is.na(savedest)) {
            savefilename <- paste0(ttl, ".jpg")
        } else {
            savefilename <- paste0(savedest, ttl, ".jpg")
        }

        if (res != 220) {
            img.url <- gsub(220, res, img.url)
        }

        download.file(img.url, stringr::str_replace_all(savefilename," ", "_"))
        return(paste0("orig.file: ", basename(img.url))) # tell user original filename (or error)
    }, res, savedest)
} 

getwikipic(titles = sps, res = 400, savedest = "data/img/")

species_consolidated <- sps_info |>
    dplyr::group_by(
        species,
        English,
        French,
        gbif_url,
        col_link,
        itis_url
    ) |>
    dplyr::summarise(
        Native_Quebec = any(native == TRUE & !is.na(Quebec)),
        Exotic_Quebec = any(exotic == TRUE & !is.na(Quebec)),
        .groups = "drop"
    ) |>
    dplyr::mutate(
        img = ifelse(file.exists(paste0("inst/extdata/img/", stringr::str_replace_all(species, " ", "_"), ".jpg")), paste0("img/", stringr::str_replace_all(species, " ", "_"), ".jpg"), NA)
    ) 

status_qc <- read.csv("inst/extdata/QC_especes_en_peril.csv")
status_ca <- read.csv("inst/extdata/CA_especes_en_peril.csv")

# Join conservation status data
# Remove duplicates from conservation status files by keeping the first occurrence
status_ca_unique <- status_ca |>
  dplyr::select(species = Nom.scientifique, status_ca = Statut.à.l.annexe.1) |>
  dplyr::distinct(species, .keep_all = TRUE)

status_qc_unique <- status_qc |>
  dplyr::select(species = Nom_scientifique, status_qc = STATUT_LEMV) |>
  dplyr::distinct(species, .keep_all = TRUE)

species_consolidated |>
    dplyr::left_join(
        status_ca_unique,
        by = c("species")
    ) |>
    dplyr::left_join(
        status_qc_unique,
        by = c("species")
    ) |>
    dplyr::mutate(
        status_ca = ifelse(status_ca == "Aucun statut", NA, status_ca),
        status_qc = ifelse(status_qc == "Non suivie", NA, status_qc)
    ) |> write.csv("inst/extdata/species_info.csv", row.names = FALSE)



library(dplyr)
library(readr)
library(stringr)

# Convert DMS string (e.g. "47°56'59.9\"N") to decimal degrees
dms_to_dd <- function(x) {
  x <- str_trim(x)
  # Extract components: degrees, minutes, seconds, hemisphere
  m <- str_match(x, r"((\d+)°(\d+)'([\d.]+)\"?\s*([NSEW]))")
  deg  <- as.numeric(m[, 2])
  min  <- as.numeric(m[, 3])
  sec  <- as.numeric(m[, 4])
  hemi <- m[, 5]
  dd   <- deg + min / 60 + sec / 3600
  ifelse(hemi %in% c("S", "W"), -dd, dd)
}

loc <- read_delim("inst/extdata/localisations.csv", delim = ";",
                  show_col_types = FALSE, trim_ws = TRUE)

loc_dd <- loc |>
  mutate(
    lat  = dms_to_dd(lat),
    long = dms_to_dd(long)
  )

loc_dd_sf <- sf::st_as_sf(loc_dd, coords = c("long", "lat"), crs = 4326)

mapview::mapview(loc_dd_sf)

write_csv(loc_dd, "inst/extdata/localisations.csv")

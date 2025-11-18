
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metabarMap

Interactive Shiny application for visualizing metabarcoding results on a
map.

## Description

This application displays metabarcoding detection data for multiple
sampling stations. Users can:

- View sampling stations on an interactive map colored by species
  richness
- Click stations to see detected species with images, common names, and
  links to external databases
- Browse ambiguous species groups that could not be distinguished
- Download all data as CSV files

## Data Structure

The application requires project-specific data files in `inst/extdata/`:

### Required CSV Files

1.  **localisations.csv** - Station coordinates
    - Columns: `station`, `lat`, `long`
    - Example: `ST1,51.6908819,-75.8168186`
2.  **species_table.csv** - Detection data (wide format)
    - Columns: `TaxonName`, `Group`, `Genus`, `Species`, `ST1`, `ST2`,
      …, `STn`
    - Each station column contains read counts (numeric)
    - Species with `Group="zMultiple"` are treated as ambiguous
      detections
    - Ambiguous species may have colon-separated names:
      `"species1 : species2"`
    - Ambiguous species may have underscore format:
      `"Group_Genus_Species"`
3.  **species_info.csv** - Species metadata
    - Columns: `species`, `English`, `French`, `gbif_url`, `col_link`,
      `itis_url`, `Native_Quebec`, `Exotic_Quebec`, `img`
    - `species`: Scientific name (Genus species)
    - `English`/`French`: Common names
    - `*_url`: Links to external databases (GBIF, Catalogue of Life,
      ITIS)
    - `Native_Quebec`/`Exotic_Quebec`: Boolean flags for species status
    - `img`: Path to species image (e.g.,
      `inst/extdata/img/Species_name.jpg`)

### Images

Species images should be placed in `inst/extdata/img/` with filenames
matching the paths in `species_info.csv`.

## Running Locally

``` r
# Install dependencies
remotes::install_local()

# Run the app
metabarMap::run_app()
```

## Docker Deployment

### Build locally for ShinyProxy and upload on the remote registry

1.  Open SSH tunnel to remote server:

``` bash
ssh -i ~/.ssh/id_compute_cloud -L 5001:localhost:5000 sviss@ip.to.compute.cloud.canada
```

2.  Build and tag the image (replace `project-tag` with your project
    identifier):

``` bash
docker build -t localhost:5001/metabarmap:project-tag .
```

3.  Push to ShinyProxy registry:

``` bash
docker push localhost:5001/metabarmap:project-tag
```

### On the remote server

Pull the image localy from the registry

``` bash
docker pull localhost:500/metabarmap:v1.0.0-nemaska
```

## Configuration

Station IDs are configured in `inst/golem-config.yml`:

``` yaml
default:
  station_ids: ["ST1","ST2","ST3",...]
  project_name: "Your Project Name"
  project_logo: "https://your-logo-url.png"
```

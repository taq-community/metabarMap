# Map Module UI
mapUI <- function(id) {
  ns <- shiny::NS(id)
  leaflet::leafletOutput(ns("map"), height = "600px")
}

# Map Module Server
mapServer <- function(id, map_data) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive value to store selected station - preselect first station
    selected_station <- shiny::reactiveVal(map_data$station[1])

    # Create color palette based on species richness
    output$map <- leaflet::renderLeaflet({

      # Define color palette
      pal <- leaflet::colorNumeric(
        palette = "YlOrRd",
        domain = map_data$n_species
      )

      leaflet::leaflet(map_data, options = leaflet::leafletOptions(maxZoom = 16)) |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = "Satellite"
        ) |>
        leaflet::addTiles(group = "OpenStreetMap") |>
        leaflet::addLayersControl(
          baseGroups = c("Satellite", "OpenStreetMap"),
          position = "topright",
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        leaflet::addScaleBar(
          position = "bottomleft",
          options = leaflet::scaleBarOptions(imperial = FALSE)
        ) |>
        leaflet::addCircleMarkers(
          lng = ~long,
          lat = ~lat,
          radius = 10,
          fillColor = ~pal(n_species),
          fillOpacity = 0.8,
          color = "white",
          weight = 2,
          layerId = ~station,
          label = ~lapply(paste0("<strong>Station ", station, "</strong><br>", n_species, " species detected"), htmltools::HTML),
          labelOptions = leaflet::labelOptions(
            style = list("padding" = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          )
        ) |>
        leaflet::addLegend(
          pal = pal,
          values = ~n_species,
          title = "Number of species detected",
          position = "bottomright",
          opacity = 0.8
        ) |>
        leaflet::setView(
          lng = mean(map_data$long),
          lat = mean(map_data$lat),
          zoom = 12
        ) |>
        # Highlight first station on load
        leaflet::addCircleMarkers(
          data = map_data[1, ],
          lng = ~long,
          lat = ~lat,
          radius = 12,
          fillColor = "transparent",
          color = "blue",
          weight = 4,
          group = "highlight"
        )
    })

    # Observe marker clicks
    shiny::observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      if (!is.null(click)) {
        selected_station(click$id)

        # Update map to highlight selected station
        leaflet::leafletProxy("map") |>
          leaflet::clearGroup("highlight") |>
          leaflet::addCircleMarkers(
            data = map_data[map_data$station == click$id, ],
            lng = ~long,
            lat = ~lat,
            radius = 12,
            fillColor = "transparent",
            color = "blue",
            weight = 4,
            group = "highlight"
          )
      }
    })

    # Observe changes to selected_station (e.g., from URL restore)
    shiny::observeEvent(selected_station(), {
      station <- selected_station()
      if (!is.null(station)) {
        # Update map to highlight selected station
        leaflet::leafletProxy("map") |>
          leaflet::clearGroup("highlight") |>
          leaflet::addCircleMarkers(
            data = map_data[map_data$station == station, ],
            lng = ~long,
            lat = ~lat,
            radius = 12,
            fillColor = "transparent",
            color = "blue",
            weight = 4,
            group = "highlight"
          )
      }
    })

    # Return selected station as reactive
    return(selected_station)
  })
}

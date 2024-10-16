###
# Shiny app for data downscaling.
# Users select geolocative data for CSV export
# using clicked points, CSV, raster, or vector files.
# Data comes from natural earth
###

library(shiny)
library(shinyjs)
library(leaflet)
library(sf)
library(sp)
library(raster)
library(terra)
library(magick)
library(rnaturalearth)
library(digest)
library(climr)
library(shinycssloaders)
library(bslib)
theme <- bs_theme(version = 5, bootswatch = "cerulean")

# Load and convert world data to sf object.
canada <- st_as_sf(ne_states(country = "Canada"))
bc <- canada[canada$name_en == "British Columbia", ]
topo_raster <- raster("GDEM-10km-BW.tif")

cache_file <- "topo_cache.rds"

if (file.exists(cache_file)) {
  cat("Loading cached raster data\n")
  topo_raster <- readRDS(cache_file)
} else {
  cat("Generating raster data\n")
  topo_raster <- raster("GDEM-10km-BW.tif")
  cat("Saving raster data\n")
  saveRDS(topo_raster, cache_file)
}

createDeleteButton <- function(id) {
  tags$button(
    "Remove",
    id = sprintf("delete_point_%s", id),
    onclick = sprintf("Shiny.onInputChange('delete_point', '%s')", id)
  )
}

generateId <- function(lon, lat) {
  digest(paste(lon, lat), algo = "md5")
}

generateMarkerPopup <- function(point) {
  as.character(
    tags$div(
      tags$p(sprintf("Longitude: %s", point$lon)),
      tags$p(sprintf("Latitude: %s", point$lat)),
      tags$p(sprintf("Elevation: %s", point$elev)),
      createDeleteButton(point$id)
    )
  )
}

ssp_years_choices <- climr::list_gcm_ssp_years()
ssp_choices <- climr::list_ssps()

# Page layout.
ui <- fluidPage(
  theme = theme,
  title = "Regional Data Downscaling",
  tags$head(
    tags$meta(
      "http-equiv" = "Content-Security-Policy",
      content = "default-src * 'unsafe-inline' 'unsafe-eval'; script-src * 'self' 'unsafe-inline' 'unsafe-eval'; style-src * 'unsafe-inline';",
      sandbox = "sandbox allow-scripts allow-forms allow-popups allow-modals; script-src 'self' 'unsafe-inline' 'unsafe-eval'; child-src 'self';"
    )
  ),
  fluidRow(
    column(
      width = 9, offset = 1,
      titlePanel("Regional Data Downscaling"),
      h3("Define points on the map or upload a file.")
    )
  ),
  fluidRow(
    column(
      width = 3, offset = 1,
      card(
        accordion(
          accordion_panel(
            id = "import",
            title = "Select",
            icon = icon("pencil"),
            card(
              fileInput(
                "file_upload",
                "Create selection from file upload",
                placeholder = "CSV, TIFF, or SHP",
                accept = c(".csv", ".tif", ".tiff", ".shp"),
                buttonLabel = "Select a file"
              )
            )
          ),
          accordion_panel(
            id = "export",
            title = "Export",
            icon = icon("download"),
            card(
              selectInput(
                "gcms",
                "GCMS",
                choices = climr::list_gcms(),
                multiple = TRUE
              ),
              selectInput(
                "ssps",
                "Shared socioeconomic pathways",
                choices = ssp_choices,
                multiple = TRUE
              ),
              selectInput(
                "gcm_periods",
                "GCM Periods",
                choices = climr::list_gcm_periods(),
              ),
              selectInput(
                "gcm_hist_years",
                "GCM Historical Years",
                choices = climr::list_gcm_hist_years(),
                multiple = TRUE
              ),
              selectInput(
                "gcm_ssp_years",
                "GCM SSP Years",
                choices = ssp_years_choices,
                multiple = TRUE
              ),
              selectInput(
                "obs_years",
                "Observed Years",
                choices = climr::list_obs_years(),
                multiple = TRUE
              ),
              selectInput(
                "vars",
                "Variables to include",
                climr::list_vars(),
                multiple = TRUE
              ),
              selectInput(
                "refMap",
                "Reference Map",
                climr::list_refmaps()
              )
            ),
            downloadButton("download", "Download CSV")
          ),
          id = "options",
        ),
      )
    ),
    column(
      width = 7,
      card(
        withSpinner(leafletOutput("map", height = 500)),
        tableOutput("points_table"),
      )
    ),
    tags$div(
      id = "progress-modal", class = "modal", style = "display: none;",
      tags$div(
        class = "modal-content",
        tags$h4("Generating Download"),
        tags$p("Please wait while the data is being processed."),
        tags$div(id = "progress-text")
      )
    )
  )
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        group = "British Columbia",
        data = bc, color = "green", weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 0.5
      ) %>%
      addLayersControl(
        overlayGroups = c("British Columbia"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  points <- reactiveValues(data = NULL)
  clicked_polygons <- reactiveValues(data = NULL)

  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    clicked_polygon <- data.frame(
      group = click$group,
      lat = click$lat,
      lon = click$lng
    )
    if (is.null(clicked_polygons$data)) {
      clicked_polygons$data <- clicked_polygon
    } else {
      clicked_polygons$data <- rbind(clicked_polygons$data, clicked_polygon)
    }
    if (is.null(clicked_polygons$data)) {
      return()
    }

    elevation <- extract(topo_raster, cbind(click$lng, click$lat))
    new_point <- data.frame(
      id = generateId(click$lng, click$lat),
      lon = click$lng,
      lat = click$lat,
      elev = elevation
    )
    if (is.null(points$data)) {
      points$data <- new_point
    } else {
      points$data <- rbind(points$data, new_point)
    }
    leafletProxy("map") %>%
      addMarkers(
        lng = new_point$lon,
        lat = new_point$lat,
        layerId = new_point$id,
        popup = generateMarkerPopup(new_point)
      )
  })

  observeEvent(input$delete_point, {
    point_id <- input$delete_point
    if (!is.null(points$data)) {
      points$data <- points$data[points$data$id != point_id, ]
      leafletProxy("map") %>%
        removeMarker(layerId = point_id)
    }
  })

  # Process impending file.
  observeEvent(input$file_upload, {
    file <- input$file_upload
    ext <- tools::file_ext(file$name)

    switch(ext,
      "csv" = {
        csv_data <- read.csv(file$datapath)
        points$data <- data.frame(
          id = csv_data$id,
          elev = csv_data$elevation,
          lon = csv_data$longitude,
          lat = csv_data$latitude
        )
        leafletProxy("map") %>%
          clearMarkers() %>%
          addMarkers(
            data = points$data,
            lng = ~lon,
            lat = ~lat,
            layerId = ~id,
            popup = generateMarkerPopup(points$data)
          )
      },
      "tif" = {
        raster_data <- raster(file$datapath)
        leafletProxy("map") %>%
          addRasterImage(raster_data, opacity = 0.8)
      },
      "shp" = {
        shp_data <- st_read(file$datapath)
        leafletProxy("map") %>%
          addPolygons(data = shp_data, color = "red", weight = 1, opacity = 0.8)
      },
      {
        warning("Unsupported file type")
      }
    )
  })

  output$points_table <- renderTable(
    {
      if (is.null(points$data)) {
        return(data.frame(
          Elevation = numeric(),
          Longitude = numeric(),
          Latitude = numeric(),
          id = character()
        ))
      }
      (
        data.frame(
          Elevation = points$data$elev,
          Longitude = points$data$lon,
          Latitude = points$data$lat,
          id = points$data$id
        )
      )
    },
    sanitize.text.function = function(x) x,
    striped = TRUE
  )

  output$download <- downloadHandler(
    filename = function() {
      paste("downscaled_geo_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      shinyjs::disable("download")
      shinyjs::show("progress-modal")
      progress <- shiny::Progress$new()
      progress$set(message = "Download CSV", detail = "Preparing", value = 0)
      on.exit({
        shinyjs::enable("download")
        shinyjs::hide("progress-modal")
      })
      tryCatch(
        {
          if (!is.null(points$data)) {
            cat("Processing points data\n")
            progress$inc(0.25, detail = "Compiling")
            downscale_data <- data.frame(
              id = points$data$id,
              lon = points$data$lon,
              lat = points$data$lat,
              elev = points$data$elev
            )
            cat("Downscaling data\n")
            print(downscale_data)
            progress$inc(0.5, detail = "Downscaling")
            downscaled_data <- downscale(
              xyz = downscale_data,
              gcms = input$gcms,
              gcm_ssp_years = input$gcm_ssp_years,
              gcm_periods = input$gcm_periods,
              vars = input$vars,
              ssps = input$ssps,
              which_refmap = input$refMap
            )
            progress$inc(0.75, detail = "Writing")
            write.csv(downscaled_data, file, row.names = FALSE)
            progress$inc(1.0, detail = "Complete")
          }
        },
        error = function(e) {
          progress$set(message = "Error", detail = e$message, value = 1)
          cat("Error: ", e$message, "\n")
        },
        finally = {
          shinyjs::enable("download")
          shinyjs::hide("progress-modal")
          progress$close()
        }
      )
    }
  )
}

shinyApp(ui = ui, server = server)

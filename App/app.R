# Load required libraries
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)

# Load and prepare data
load_data <- function() {
  # Burglary data canton ZH
  ebd <- read.csv("../Data/KTZH_EBD.csv")

  # Remove unnecessary records
  ebd <- ebd %>%
    filter(Tatbestand == 'Einbrüche insgesamt',
           Gemeindename != "unbekannt ZH",
           Stadtkreis_Name != "unbekannt") %>%
    select(-c(Gesetz_Nummer, Gesetz_Abk))

  # Income data on municipality level
  income_kt <- read.csv("../Data/KTZH_Income_median.csv")

  # Remove non-municipality and city data and unused columns
  income_kt <- income_kt %>%
    filter(!BFS_NR %in% c(0, 261)) %>%
    select(BFS_NR, GEBIET_NAME, INDIKATOR_JAHR, INDIKATOR_VALUE) %>%
    rename(INCOME_VALUE = INDIKATOR_VALUE)

  # Income data on city district level
  income_st <- read.csv("../Data/STZH_Income_median.csv")

  income_st <- income_st %>%
    filter(SteuerTarifLang == "Grundtarif") %>%
    select(-c("KreisSort", "KreisCd", "SteuerTarifSort",
              "SteuerTarifCd", "SteuerTarifLang")) %>%
    mutate(INCOME_VALUE = SteuerEinkommen_p50 * 1000)

  # Merge data sets
  data <- ebd %>%
    left_join(income_kt,
              by = c("Gemeinde_BFS_Nr" = "BFS_NR",
                     "Ausgangsjahr" = "INDIKATOR_JAHR")) %>%
    left_join(income_st %>%
                select(KreisLang, StichtagDatJahr, INCOME_VALUE) %>%
                rename(INCOME_VALUE_ST = INCOME_VALUE),
              by = c("Stadtkreis_Name" = "KreisLang",
                     "Ausgangsjahr" = "StichtagDatJahr")) %>%
    mutate(INCOME_VALUE = if_else(!is.na(INCOME_VALUE_ST),
                                  INCOME_VALUE_ST,
                                  INCOME_VALUE)) %>%
    select(-INCOME_VALUE_ST)

  # Read .gpkg of municipalities
  ktzh_gpkg <- st_read("../Data/KTZH_Gemeindegrenzen_OGD.gpkg",
                       layer = "UP_GEMEINDEN_SEEN_F", quiet = TRUE)

  # Remove non-municipality and city geoms
  ktzh_geom <- ktzh_gpkg %>%
    filter(!BFS %in% c(0, 261), ART_CODE == 1) %>%
    select(BFS, geom)

  # Read .gpkg of swiss borders (swisstopo)
  swiss_borders <- st_read("../Data/CH_Borders_swisstopo.gpkg",
                           layer = "tlm_landesgebiet", quiet = TRUE)

  swiss_borders <- swiss_borders %>%
    filter(icc == "CH")

  # Calculate distance to border
  swiss_border_line <- st_boundary(swiss_borders)

  # Ensure same CRS
  ktzh_geom <- st_transform(ktzh_geom, st_crs(swiss_borders))

  # Calculate distance to border
  ktzh_geom <- ktzh_geom %>%
    st_transform(st_crs(swiss_borders)) %>%
    mutate(
      distance_to_border = st_distance(st_centroid(.), swiss_border_line)[,1],
      distance_to_border_km = as.numeric(distance_to_border) / 1000
    ) %>%
    select(BFS, geom, distance_to_border, distance_to_border_km)

  # Convert city district .gpkg CURVEPOLYGON TO MULTIPOLYGON
  if (!file.exists("../Data/STZH_lin.gpkg")) {
    gdal_utils(
      util = "vectortranslate",
      source = "../Data/STZH_Stadtkreise_OGD.gpkg",
      destination = "../Data/STZH_lin.gpkg",
      options = c("-nlt", "MULTIPOLYGON",
                  "-lco", "GEOMETRY_NAME=geom",
                  "-overwrite")
    )
  }

  # Read the converted file
  stzh_gpkg <- st_read("../Data/STZH_lin.gpkg", quiet = TRUE)

  stzh_geom <- stzh_gpkg %>%
    select(kname, geom) %>%
    st_transform(st_crs(swiss_borders)) %>%
    mutate(
      distance_to_border = st_distance(st_centroid(.), swiss_border_line)[,1],
      distance_to_border_km = as.numeric(distance_to_border) / 1000
    )

  # Join geoms of the municipalities
  data <- data %>%
    left_join(ktzh_geom, by = c("Gemeinde_BFS_Nr" = "BFS"))

  # Convert data's geometry to MULTIPOLYGON first
  data <- data %>%
    mutate(geom = st_cast(geom, "MULTIPOLYGON"))

  # Join and replace only the EMPTY geometries
  data <- data %>%
    left_join(
      stzh_geom %>%
        select(kname,
               geom_stzh = geom,
               distance_to_border_stzh = distance_to_border,
               distance_to_border_km_stzh = distance_to_border_km),
      by = c("Stadtkreis_Name" = "kname")
    ) %>%
    mutate(
      geom = if_else(st_is_empty(geom) & !is.na(geom_stzh),
                     geom_stzh,
                     geom),
      distance_to_border = coalesce(distance_to_border_stzh, distance_to_border),
      distance_to_border_km = coalesce(distance_to_border_km_stzh, distance_to_border_km)
    ) %>%
    select(-geom_stzh, -distance_to_border_stzh, -distance_to_border_km_stzh) %>%
    st_as_sf() %>%
    mutate(Gemeinde_BFS_Nr = if_else(Gemeindename == "Zürich",
                                     Stadtkreis_BFS_Nr,
                                     Gemeinde_BFS_Nr))

  # Transform to WGS84 for leaflet
  data <- st_transform(data, 4326)

  return(data)
}

# Load data once at startup
data <- load_data()

# Define UI
ui <- fluidPage(
  titlePanel("Canton Zurich - Income and Burglary Analysis"),

  tabsetPanel(
    tabPanel("Map",
             fluidRow(
               column(3,
                      wellPanel(
                        h4("Map Controls"),
                        selectInput("variable",
                                    "Select Variable:",
                                    choices = c("Median Income" = "INCOME_VALUE",
                                                "Population" = "Einwohner",
                                                "Total Burglaries" = "Straftaten_total",
                                                "Distance to Border (km)" = "distance_to_border_km"),
                                    selected = "INCOME_VALUE"),
                        uiOutput("year_slider"),
                        hr(),
                        checkboxInput("exclude_kreis1",
                                      "Exclude Kreis 1 (outlier)",
                                      value = FALSE),
                        hr(),
                        h5("Map Information"),
                        textOutput("info_text")
                      )
               ),
               column(9,
                      leafletOutput("map", height = "700px")
               )
             )
    ),

    tabPanel("About",
             fluidRow(
               column(12,
                      h3("Research Question"),
                      p("How does income level and proximity to national borders relate to burglary rates
                        across municipalities in Canton Zurich?"),
                      h3("Hypotheses"),
                      tags$ul(
                        tags$li(strong("H1:"), "Municipalities with higher median income exhibit higher burglary rates"),
                        tags$li(strong("H2:"), "Municipalities closer to national borders experience higher burglary rates")
                      ),
                      h3("Data Sources"),
                      tags$ul(
                        tags$li("Canton Zurich burglary statistics"),
                        tags$li("Canton Zurich median income data"),
                        tags$li("City of Zurich district-level income data"),
                        tags$li("Swisstopo Swiss border geodata")
                      )
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Dynamic year slider based on selected variable
  output$year_slider <- renderUI({
    req(input$variable)

    years <- sort(unique(data$Ausgangsjahr))

    sliderInput("year",
                "Select Year:",
                min = min(years),
                max = max(years),
                value = max(years),
                step = 1,
                sep = "")
  })

  # Filter data based on inputs
  filtered_data <- reactive({
    req(input$year, input$variable)

    df <- data %>%
      filter(Ausgangsjahr == input$year)

    # Exclude Kreis 1 if checkbox is checked
    if (input$exclude_kreis1) {
      df <- df %>%
        filter(Gemeinde_BFS_Nr != 2610001)
    }

    # Remove rows with NA for the selected variable
    df <- df %>%
      filter(!is.na(.data[[input$variable]]))

    df
  })

  # Create color palette
  color_palette <- reactive({
    req(input$variable, filtered_data())

    df <- filtered_data()

    # Check if we have valid data
    if (nrow(df) == 0) {
      return(NULL)
    }

    values <- df[[input$variable]]

    # Remove NA values for domain calculation
    values_clean <- values[!is.na(values)]

    # Check if we have any valid values
    if (length(values_clean) == 0) {
      return(NULL)
    }

    colorNumeric(
      palette = "viridis",
      domain = values_clean,
      na.color = "#808080"
    )
  })

  # Info text
  output$info_text <- renderText({
    req(input$year)
    df <- filtered_data()
    paste0("Showing ", nrow(df), " municipalities/districts for year ", input$year)
  })

  # Render leaflet map
  output$map <- renderLeaflet({
    # Base map that doesn't change
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 8.55, lat = 47.37, zoom = 10)
  })

  # Observer to update map when data or variable changes
  observe({
    req(filtered_data(), input$variable, color_palette())

    df <- filtered_data()
    pal <- color_palette()

    # Check if we have data to display
    if (nrow(df) == 0 || is.null(pal)) {
      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls()
      return()
    }

    # Variable label for legend
    var_label <- switch(input$variable,
                        "INCOME_VALUE" = "Median Income (CHF)",
                        "Einwohner" = "Population",
                        "Straftaten_total" = "Total Burglaries",
                        "distance_to_border_km" = "Distance to Border (km)")

    # Create labels for popups
    labels <- sprintf(
      "<strong>%s</strong><br/>%s: %s",
      ifelse(!is.na(df$Stadtkreis_Name),
             paste0(df$Gemeindename, " - ", df$Stadtkreis_Name),
             df$Gemeindename),
      var_label,
      format(round(df[[input$variable]], 1), big.mark = "'")
    ) %>% lapply(htmltools::HTML)

    leafletProxy("map", data = df) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal(df[[input$variable]]),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = df[[input$variable]],
        title = var_label,
        opacity = 0.7
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

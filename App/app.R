# Load required libraries
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(ggplot2)

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

# Calculate years with complete data
# Use all years where burglary data exists, since income data is generally complete
# The data structure changes over time due to municipal mergers, but each year is self-contained
all_years <- sort(unique(data$Ausgangsjahr))

# Calculate coverage percentage for each year to identify truly incomplete years
year_coverage <- data %>%
  st_drop_geometry() %>%
  group_by(Ausgangsjahr) %>%
  summarise(
    total_units = n(),
    units_with_income = sum(!is.na(INCOME_VALUE)),
    coverage_pct = (units_with_income / total_units) * 100,
    .groups = "drop"
  )

# Keep years with at least 95% income data coverage
complete_years <- year_coverage %>%
  filter(coverage_pct >= 95) %>%
  pull(Ausgangsjahr) %>%
  sort()

# If no years meet the threshold, use all years
if (length(complete_years) == 0) {
  complete_years <- all_years
}

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
                                                "Total Burglaries" = "Straftaten_total"),
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

    tabPanel("Data Explorer",
             fluidRow(
               column(3,
                      wellPanel(
                        h4("Explorer Controls"),
                        selectInput("selected_location",
                                    "Select Municipality/District:",
                                    choices = NULL),
                        uiOutput("explorer_year_slider"),
                        hr(),
                        h5("Location Information"),
                        htmlOutput("location_info")
                      )
               ),
               column(9,
                      fluidRow(
                        column(6,
                               plotOutput("income_plot", height = "300px")
                        ),
                        column(6,
                               plotOutput("burglary_plot", height = "300px")
                        )
                      ),
                      hr(),
                      fluidRow(
                        column(6,
                               plotOutput("population_plot", height = "300px")
                        ),
                        column(6,
                               plotOutput("burglary_rate_plot", height = "300px")
                        )
                      )
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

  # Dynamic year slider for map - only years with complete data
  output$year_slider <- renderUI({
    req(input$variable)

    if (length(complete_years) == 0) {
      return(NULL)
    }

    sliderInput("year",
                "Select Year:",
                min = min(complete_years),
                max = max(complete_years),
                value = max(complete_years),
                step = 1,
                sep = "")
  })

  # Update location choices for Data Explorer
  observe({
    location_choices <- data %>%
      st_drop_geometry() %>%
      mutate(
        display_name = if_else(
          !is.na(Stadtkreis_Name),
          paste0(Gemeindename, " - ", Stadtkreis_Name),
          Gemeindename
        )
      ) %>%
      distinct(Gemeinde_BFS_Nr, display_name) %>%
      arrange(display_name)

    choices <- setNames(location_choices$Gemeinde_BFS_Nr,
                       location_choices$display_name)

    updateSelectInput(session, "selected_location",
                     choices = choices,
                     selected = choices[1])
  })

  # Dynamic year slider for Data Explorer - all available years
  output$explorer_year_slider <- renderUI({
    req(input$selected_location)

    location_data <- data %>%
      st_drop_geometry() %>%
      filter(Gemeinde_BFS_Nr == input$selected_location)

    years <- sort(unique(location_data$Ausgangsjahr))

    if (length(years) == 0) {
      return(NULL)
    }

    sliderInput("explorer_year",
                "Select Year Range:",
                min = min(years),
                max = max(years),
                value = c(min(years), max(years)),
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

  # Data Explorer reactive data
  explorer_data <- reactive({
    req(input$selected_location, input$explorer_year)

    data %>%
      st_drop_geometry() %>%
      filter(
        Gemeinde_BFS_Nr == input$selected_location,
        Ausgangsjahr >= input$explorer_year[1],
        Ausgangsjahr <= input$explorer_year[2]
      ) %>%
      arrange(Ausgangsjahr)
  })

  # Location information text
  output$location_info <- renderUI({
    req(input$selected_location)

    location_info <- data %>%
      st_drop_geometry() %>%
      filter(Gemeinde_BFS_Nr == input$selected_location) %>%
      slice(1)

    location_name <- if_else(
      !is.na(location_info$Stadtkreis_Name),
      paste0(location_info$Gemeindename, " - ", location_info$Stadtkreis_Name),
      location_info$Gemeindename
    )

    distance_km <- round(location_info$distance_to_border_km, 1)

    HTML(paste0(
      "<strong>Location:</strong> ", location_name, "<br/>",
      "<strong>BFS Number:</strong> ", location_info$Gemeinde_BFS_Nr, "<br/>",
      "<strong>Distance to Border:</strong> ", distance_km, " km"
    ))
  })

  # Income plot
  output$income_plot <- renderPlot({
    req(explorer_data())
    df <- explorer_data()

    if (nrow(df) == 0 || all(is.na(df$INCOME_VALUE))) {
      plot.new()
      text(0.5, 0.5, "No income data available", cex = 1.5)
      return()
    }

    ggplot(df, aes(x = Ausgangsjahr, y = INCOME_VALUE)) +
      geom_line(color = "#440154", size = 1.2) +
      geom_point(color = "#440154", size = 3) +
      labs(
        title = "Median Income Over Time",
        x = "Year",
        y = "Median Income (CHF)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 11)
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = unique(df$Ausgangsjahr))
  })

  # Burglary plot
  output$burglary_plot <- renderPlot({
    req(explorer_data())
    df <- explorer_data()

    if (nrow(df) == 0 || all(is.na(df$Straftaten_total))) {
      plot.new()
      text(0.5, 0.5, "No burglary data available", cex = 1.5)
      return()
    }

    ggplot(df, aes(x = Ausgangsjahr, y = Straftaten_total)) +
      geom_line(color = "#31688e", size = 1.2) +
      geom_point(color = "#31688e", size = 3) +
      labs(
        title = "Total Burglaries Over Time",
        x = "Year",
        y = "Total Burglaries"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 11)
      ) +
      scale_x_continuous(breaks = unique(df$Ausgangsjahr))
  })

  # Population plot
  output$population_plot <- renderPlot({
    req(explorer_data())
    df <- explorer_data()

    if (nrow(df) == 0 || all(is.na(df$Einwohner))) {
      plot.new()
      text(0.5, 0.5, "No population data available", cex = 1.5)
      return()
    }

    ggplot(df, aes(x = Ausgangsjahr, y = Einwohner)) +
      geom_line(color = "#35b779", size = 1.2) +
      geom_point(color = "#35b779", size = 3) +
      labs(
        title = "Population Over Time",
        x = "Year",
        y = "Population"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 11)
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = unique(df$Ausgangsjahr))
  })

  # Burglary rate plot (per 1000 residents)
  output$burglary_rate_plot <- renderPlot({
    req(explorer_data())
    df <- explorer_data() %>%
      mutate(burglary_rate = (Straftaten_total / Einwohner) * 1000)

    if (nrow(df) == 0 || all(is.na(df$burglary_rate))) {
      plot.new()
      text(0.5, 0.5, "No data available", cex = 1.5)
      return()
    }

    ggplot(df, aes(x = Ausgangsjahr, y = burglary_rate)) +
      geom_line(color = "#fde724", size = 1.2) +
      geom_point(color = "#fde724", size = 3) +
      labs(
        title = "Burglary Rate Over Time",
        x = "Year",
        y = "Burglaries per 1,000 Residents"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 11)
      ) +
      scale_x_continuous(breaks = unique(df$Ausgangsjahr))
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

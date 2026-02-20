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
  ebd_raw <- read.csv("../Data/KTZH_EBD.csv")

  # Full ebd for visualization functions (all Tatbestand types)
  ebd_viz <- ebd_raw %>%
    filter(Gemeindename != "unbekannt ZH",
           Stadtkreis_Name != "unbekannt") %>%
    select(-c(Gesetz_Nummer, Gesetz_Abk))

  # Filtered ebd for map data (Einbrüche insgesamt only)
  ebd <- ebd_viz %>%
    filter(Tatbestand == 'Einbrüche insgesamt')

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

  # Save raw income_kt before merger handling (for merger visualization)
  income_kt_raw <- income_kt

  # Define merger mapping
  merger_mapping <- tribble(
    ~old_BFS_Nr, ~new_BFS_Nr, ~merge_year,
    # Stammheim merger (2019)
    36,          292,          2019,
    42,          292,          2019,
    44,          292,          2019,
    # Wädenswil merger (2019)
    134,         293,          2019,
    140,         293,          2019,
    142,         293,          2019,
    # Elgg merger (2018)
    217,         294,          2018,
    222,         294,          2018,
    # Horgen merger (2018)
    132,         295,          2018,
    133,         295,          2018,
    # Illnau-Effretikon merger (2016)
    174,         296,          2016,
    175,         296,          2016,
    # Bauma merger (2015)
    171,         297,          2015,
    179,         297,          2015
  )
  
  all_synthetic <- list()
  
  years <- sort(unique(income_kt$INDIKATOR_JAHR))
  
  for (bfs in unique(merger_mapping$new_BFS_Nr)) {
    
    old_bfs <- merger_mapping %>% 
      filter(new_BFS_Nr == bfs) %>% 
      pull(old_BFS_Nr)
    
    bfs_years <- income_kt %>%
      filter(BFS_NR == bfs) %>%
      pull(INDIKATOR_JAHR) %>%
      unique()
    
    missing_years <- setdiff(years, bfs_years)
    
    if (length(missing_years) > 0) {
      
      synthetic <- income_kt %>% 
        filter(BFS_NR %in% old_bfs,
               INDIKATOR_JAHR %in% missing_years) %>%
        group_by(INDIKATOR_JAHR) %>%
        summarise(
          INCOME_VALUE = mean(INCOME_VALUE, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(BFS_NR = bfs)
      
      all_synthetic[[as.character(bfs)]] <- synthetic
    }
  }
  
  income_old_bfs_all <- bind_rows(all_synthetic) %>%
    left_join(
      income_kt %>%
        select(BFS_NR, GEBIET_NAME) %>%
        distinct(),
      by = "BFS_NR"
    )
  
  # Remove ALL old municipalities
  income_kt <- income_kt %>%
    filter(!BFS_NR %in% merger_mapping$old_BFS_Nr)
  
  # Add synthetic rows
  income_kt <- income_kt %>%
    bind_rows(income_old_bfs_all) %>%
    arrange(BFS_NR, INDIKATOR_JAHR)
  
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

  return(list(
    data           = data,
    ebd_viz        = ebd_viz,
    income_kt_viz  = income_kt,
    income_kt_raw  = income_kt_raw,
    income_st_viz  = income_st,
    merger_mapping = merger_mapping
  ))
}

# Load data once at startup
loaded         <- load_data()
data           <- loaded$data
ebd_viz        <- loaded$ebd_viz
income_kt_viz  <- loaded$income_kt_viz
income_kt_raw  <- loaded$income_kt_raw
income_st_viz  <- loaded$income_st_viz
merger_mapping <- loaded$merger_mapping

# Visualization functions
plot_burglary_lines <- function(data, municipality, stadtkreis = NULL) {
  df <- data %>% filter(Gemeindename == municipality)
  if (!is.null(stadtkreis)) df <- df %>% filter(Stadtkreis_Name == stadtkreis)
  title_label <- if (!is.null(stadtkreis)) paste0(municipality, " - ", stadtkreis) else municipality
  df <- df %>% mutate(Tatbestand = factor(Tatbestand, levels = c(
    "Einbrüche insgesamt", "Einbruchdiebstahl", "Einschleichdiebstahl")))
  ggplot(df, aes(Ausgangsjahr, Straftaten_total, colour = Tatbestand, linewidth = Tatbestand)) +
    geom_line() + geom_point(size = 2) +
    scale_color_manual(values = c("Einbrüche insgesamt" = "#D6604D", "Einbruchdiebstahl" = "#4393C3", "Einschleichdiebstahl" = "#9B59B6"),
                       labels = c("Einbrüche insgesamt" = "Burglaries (Total)", "Einbruchdiebstahl" = "Burglary", "Einschleichdiebstahl" = "Theft After Intrusion")) +
    scale_linewidth_manual(values = c("Einbrüche insgesamt" = 1.5, "Einbruchdiebstahl" = 0.8, "Einschleichdiebstahl" = 0.8)) +
    guides(colour = guide_legend(override.aes = list(linewidth = c(1.5, 0.8, 0.8)), nrow = 1), linewidth = "none") +
    scale_x_continuous(breaks = unique(df$Ausgangsjahr)) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
    labs(title = paste("Burglary Trends:", title_label), x = "", y = "Number of Cases", colour = "Type") +
    theme_bw(base_size = 12) + theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y = element_text(size = 11),
      legend.text  = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.position = c(0.5, 0.97),
      legend.justification = c(0.5, 1),
      legend.direction = "horizontal",
      legend.background = element_rect(fill = alpha("white", 0.8), colour = NA)
    )
}

plot_income <- function(kt_data, st_data, municipality, stadtkreis = NULL) {
  if (!is.null(stadtkreis)) {
    df <- st_data %>% filter(KreisLang == stadtkreis) %>% rename(INDIKATOR_JAHR = StichtagDatJahr)
    title_label <- paste0(municipality, " - ", stadtkreis)
  } else {
    df <- kt_data %>% filter(GEBIET_NAME == municipality)
    title_label <- municipality
  }
  ggplot(df, aes(x = INDIKATOR_JAHR, y = INCOME_VALUE)) +
    geom_line(colour = "#D4A017", linewidth = 1.5) + geom_point(colour = "#D4A017", size = 2) +
    scale_x_continuous(breaks = unique(df$INDIKATOR_JAHR)) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste("Median Income:", title_label), x = "", y = "Median Income (CHF)") +
    theme_bw(base_size = 12) + theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y = element_text(size = 11)
    )
}

plot_income_merged <- function(kt_data, kt_raw, mergers, municipality) {
  new_bfs <- kt_data %>% filter(GEBIET_NAME == municipality) %>% pull(BFS_NR) %>% first()
  merger_info <- mergers %>% filter(new_BFS_Nr == new_bfs)
  if (nrow(merger_info) == 0) return(plot_income(kt_data, NULL, municipality))
  merge_year <- merger_info$merge_year[1]
  old_bfs_nrs <- merger_info$old_BFS_Nr
  old_data <- kt_raw %>% filter(BFS_NR %in% old_bfs_nrs, INDIKATOR_JAHR < merge_year) %>%
    select(INDIKATOR_JAHR, INCOME_VALUE, label = GEBIET_NAME) %>%
    mutate(label = gsub("\\bbis\\b", "until", label), line_type = "pre-merger")
  new_data <- kt_data %>% filter(GEBIET_NAME == municipality, INDIKATOR_JAHR >= merge_year) %>%
    select(INDIKATOR_JAHR, INCOME_VALUE, label = GEBIET_NAME) %>% mutate(line_type = "post-merger")
  last_old <- old_data %>% group_by(label) %>% slice_max(INDIKATOR_JAHR) %>% ungroup()
  new_at_merge <- new_data %>% filter(INDIKATOR_JAHR == min(INDIKATOR_JAHR)) %>% select(INDIKATOR_JAHR, INCOME_VALUE)
  connector_data <- bind_rows(
    last_old %>% select(label, INDIKATOR_JAHR, INCOME_VALUE) %>% mutate(line_type = "pre-merger"),
    last_old %>% select(label) %>% mutate(INDIKATOR_JAHR = new_at_merge$INDIKATOR_JAHR, INCOME_VALUE = new_at_merge$INCOME_VALUE, line_type = "pre-merger")
  )
  level_order <- c(municipality, unique(old_data$label))
  lw_overrides <- ifelse(level_order == municipality, 1.5, 0.8)
  plot_data <- bind_rows(old_data, new_data) %>% mutate(label = factor(label, levels = level_order))
  connector_data <- connector_data %>% mutate(label = factor(label, levels = level_order))
  all_years <- sort(unique(plot_data$INDIKATOR_JAHR))
  old_labels  <- unique(old_data$label)
  old_palette <- c("#C06B5A", "#6B9E78", "#6B7FA3", "#A3788C")
  color_values <- c(setNames("#D4A017", municipality),
                    setNames(old_palette[seq_along(old_labels)], old_labels))
  ggplot(plot_data, aes(x = INDIKATOR_JAHR, y = INCOME_VALUE, colour = label, linewidth = line_type)) +
    geom_line() + geom_point(data = plot_data, size = 2) +
    geom_line(data = connector_data, linetype = "dotted") +
    geom_vline(xintercept = merge_year, linetype = "dashed", colour = "grey50") +
    annotate("text", x = merge_year, y = -Inf, label = paste("Merger", merge_year), hjust = -0.1, vjust = -1.5, colour = "grey40", size = 3) +
    scale_color_manual(values = color_values) +
    scale_linewidth_manual(values = c("pre-merger" = 0.8, "post-merger" = 1.5)) +
    scale_x_continuous(breaks = all_years) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0.05, 0.2))) +
    guides(colour = guide_legend(override.aes = list(linewidth = lw_overrides), nrow = 1), linewidth = "none") +
    labs(title = paste("Median Income:", municipality, "(incl. Municipalities Before Merger)"), x = "", y = "Median Income (CHF)", colour = "Municipality") +
    theme_bw(base_size = 12) + theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y = element_text(size = 11),
      legend.text  = element_text(size = 11),
      legend.title = element_text(size = 11),
      legend.position = c(0.5, 0.97),
      legend.justification = c(0.5, 1),
      legend.direction = "horizontal",
      legend.background = element_rect(fill = alpha("white", 0.8), colour = NA)
    )
}

plot_population <- function(data, municipality, stadtkreis = NULL) {
  df <- data %>% filter(Gemeindename == municipality)
  if (!is.null(stadtkreis)) df <- df %>% filter(Stadtkreis_Name == stadtkreis)
  df <- df %>% distinct(Ausgangsjahr, .keep_all = TRUE)
  title_label <- if (!is.null(stadtkreis)) paste0(municipality, " - ", stadtkreis) else municipality
  ggplot(df, aes(x = Ausgangsjahr, y = Einwohner)) +
    geom_line(colour = "#4DAC26", linewidth = 1.5) + geom_point(colour = "#4DAC26", size = 2) +
    scale_x_continuous(breaks = unique(df$Ausgangsjahr)) + scale_y_continuous(labels = scales::comma) +
    labs(title = paste("Population:", title_label), x = "", y = "Population") +
    theme_bw(base_size = 12) + theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y = element_text(size = 11)
    )
}

plot_ebd_rate <- function(data, municipality, stadtkreis = NULL) {
  df <- data %>% filter(Gemeindename == municipality, Tatbestand == "Einbrüche insgesamt")
  if (!is.null(stadtkreis)) df <- df %>% filter(Stadtkreis_Name == stadtkreis)
  df <- df %>% mutate(ebd_rate = (Straftaten_total / Einwohner) * 1000)
  title_label <- if (!is.null(stadtkreis)) paste0(municipality, " - ", stadtkreis) else municipality
  ggplot(df, aes(x = Ausgangsjahr, y = ebd_rate)) +
    geom_line(colour = "#D6604D", linewidth = 1.5) + geom_point(colour = "#D6604D", size = 2) +
    scale_x_continuous(breaks = unique(df$Ausgangsjahr)) +
    labs(title = paste("Burglary Rate:", title_label), x = "", y = "Burglaries per 1,000 Inhabitants") +
    theme_bw(base_size = 12) + theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y = element_text(size = 11)
    )
}

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
  tags$head(
    tags$style(HTML("
      #map { height: calc(100vh - 160px) !important; }
      #population_plot, #income_plot, #burglary_plot, #burglary_rate_plot {
        height: calc((100vh - 165px) / 2) !important;
      }
    "))
  ),
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
                                                "Burglary Rate (per 1,000 Inhabitants)" = "EBD_pop_ratio"),
                                    selected = "INCOME_VALUE"),
                        uiOutput("year_slider"),
                        hr(),
                        uiOutput("exclude_checkbox"),
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
                        hr(),
                        h5("Location Information"),
                        htmlOutput("location_info")
                      )
               ),
               column(9,
                      fluidRow(
                        column(6,
                               plotOutput("population_plot", height = "300px")
                        ),
                        column(6,
                               plotOutput("income_plot", height = "300px")
                        )
                      ),
                      hr(),
                      fluidRow(
                        column(6,
                               plotOutput("burglary_plot", height = "300px")
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
               column(8,
                      h3("About This App"),
                      p("This interactive web application was developed as part of an R Bootcamp project at the
                        Hochschule Luzern (HSLU). While the accompanying academic report examines the statistical
                        relationship between income, border proximity, and burglary rates in Canton Zurich, this app
                        takes a different angle — placing the emphasis on", strong("interactivity, exploration, and
                        the development of these variables over time."), "Depending on the dataset, the data spans from 1999 to 2024 and covers all
                        municipalities in the canton, with the city of Zurich broken down into its 12 districts for
                        finer-grained analysis."),
                      p("The report focuses on a cross-sectional analysis, but the data tells a richer story when
                        explored dynamically. How has burglary evolved in a given municipality over 15 years? Has
                        income grown faster in some areas than others? Use the", strong("Map"), "tab to compare
                        variables across the canton for any given year, and the", strong("Data Explorer"), "tab to
                        dive into the trajectory of individual municipalities and districts."),

                      h3("Research Question"),
                      p("How does income level and proximity to Switzerland's national borders relate to burglary
                        rates across municipalities in Canton Zurich?"),

                      h3("Hypotheses"),
                      p("The analysis was motivated by anecdotal observations from the Canton of Basel-Landschaft,
                        where burglaries appeared to occur more frequently in wealthier municipalities near the
                        national border. We test whether similar patterns hold in Canton Zurich:"),
                      tags$ul(
                        tags$li(strong("H1 — Income:"), "Municipalities with higher median income exhibit higher
                                burglary rates, as wealthier areas may present more attractive targets."),
                        tags$li(strong("H2 — Border proximity:"), "Municipalities located closer to national borders
                                experience higher burglary rates, potentially due to easier escape routes across
                                international boundaries.")
                      ),

                      h3("Notable Features"),
                      tags$ul(
                        tags$li(strong("Municipal mergers:"), "Six mergers between 2015 and 2019 changed
                                administrative boundaries mid-series. Pre-merger income data are reconstructed
                                from constituent municipalities and visualised separately in the Data Explorer."),
                        tags$li(strong("Zurich city districts:"), "The city of Zurich (BFS 261) is disaggregated
                                into its 12 Stadtkreise, each treated as an independent unit with its own
                                income, population, and burglary records."),
                        tags$li(strong("Outlier — Kreis 1:"), "The Zurich city centre (Kreis 1) is a strong
                                outlier in burglary rate per capita and can be excluded via the map controls
                                for a more balanced view of the canton.")
                      )
               ),
               column(4,
                      h3("Data Sources"),
                      tags$ul(
                        tags$li(strong("Burglary statistics"), br(),
                                "Canton Zurich, 2009–2024", br(),
                                tags$small("opendata.swiss")),
                        tags$li(strong("Median income — municipalities"), br(),
                                "Canton Zurich, 1999–2022", br(),
                                tags$small("zh.ch Datenkatalog")),
                        tags$li(strong("Median income — city districts"), br(),
                                "City of Zurich, 1999–2023", br(),
                                tags$small("data.stadt-zuerich.ch")),
                        tags$li(strong("Municipality boundaries"), br(),
                                "Canton Zurich GeoPackage", br(),
                                tags$small("geo.zh.ch")),
                        tags$li(strong("City district boundaries"), br(),
                                "City of Zurich GeoPackage", br(),
                                tags$small("geo.zh.ch")),
                        tags$li(strong("National border"), br(),
                                "Swiss border polygon", br(),
                                tags$small("swisstopo"))
                      ),
                      hr(),
                      h3("Authors"),
                      p("Thilo Holstein & Hans Thalathara"),
                      p(tags$small("R Bootcamp 2026 — Hochschule Luzern (HSLU)")),
                      hr(),
                      h3("Built With"),
                      tags$ul(
                        tags$li("R / Shiny"),
                        tags$li("leaflet — interactive maps"),
                        tags$li("sf — spatial data processing"),
                        tags$li("ggplot2 — data visualisation"),
                        tags$li("dplyr — data wrangling")
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

  # Reset checkbox and update label when variable changes
  observeEvent(input$variable, {
    label <- if (input$variable %in% c("Einwohner", "Straftaten_total")) {
      "Exclude Winterthur (outlier)"
    } else {
      "Exclude Kreis 1 (outlier)"
    }
    updateCheckboxInput(session, "exclude_outlier", label = label, value = FALSE)
  })

  # Dynamic checkbox depending on selected variable
  output$exclude_checkbox <- renderUI({
    req(input$variable)
    if (input$variable == "INCOME_VALUE") return(NULL)
    label <- if (input$variable %in% c("Einwohner", "Straftaten_total")) {
      "Exclude Winterthur (outlier)"
    } else {
      "Exclude Kreis 1 (outlier)"
    }
    checkboxInput("exclude_outlier", label, value = FALSE)
  })

  # Update location choices for Data Explorer
  observe({
    location_choices <- data %>%
      st_drop_geometry() %>%
      mutate(
        display_name = if_else(
          !is.na(Stadtkreis_Name) & Stadtkreis_Name != "",
          paste0(Gemeindename, " - ", Stadtkreis_Name),
          Gemeindename
        )
      ) %>%
      distinct(Gemeinde_BFS_Nr, display_name) %>%
      mutate(
        sort_name = if_else(grepl("^Zürich - Kreis", display_name), "Zürich", display_name),
        kreis_num = if_else(grepl("^Zürich - Kreis", display_name),
                            as.integer(gsub(".*Kreis ", "", display_name)),
                            NA_integer_)
      ) %>%
      arrange(sort_name, kreis_num) %>%
      select(-sort_name, -kreis_num)

    choices <- setNames(location_choices$Gemeinde_BFS_Nr,
                       location_choices$display_name)

    updateSelectInput(session, "selected_location",
                     choices = choices,
                     selected = choices[1])
  })

  # Filter data based on inputs
  filtered_data <- reactive({
    req(input$year, input$variable)

    df <- data %>%
      filter(Ausgangsjahr == input$year) %>%
      mutate(EBD_pop_ratio = (Straftaten_total / Einwohner) * 1000)

    # Exclude outlier depending on selected variable
    if (isTRUE(input$exclude_outlier)) {
      if (input$variable %in% c("Einwohner", "Straftaten_total")) {
        df <- df %>% filter(Gemeinde_BFS_Nr != 230)
      } else {
        df <- df %>% filter(Gemeinde_BFS_Nr != 2610001)
      }
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

  # Helper: resolve municipality name and stadtkreis from BFS number
  loc_info <- reactive({
    req(input$selected_location)
    info <- data %>%
      st_drop_geometry() %>%
      filter(Gemeinde_BFS_Nr == as.integer(input$selected_location)) %>%
      slice(1)
    list(
      municipality = info$Gemeindename,
      stadtkreis   = if (!is.na(info$Stadtkreis_Name) && info$Stadtkreis_Name != "") info$Stadtkreis_Name else NULL,
      is_merged    = as.integer(input$selected_location) %in% merger_mapping$new_BFS_Nr
    )
  })

  # Location information text
  output$location_info <- renderUI({
    req(input$selected_location)

    bfs_selected <- as.integer(input$selected_location)

    location_rows <- data %>%
      st_drop_geometry() %>%
      filter(Gemeinde_BFS_Nr == bfs_selected) %>%
      arrange(Ausgangsjahr)

    location_first <- slice(location_rows, 1)

    location_name <- if_else(
      !is.na(location_first$Stadtkreis_Name) & location_first$Stadtkreis_Name != "",
      paste0(location_first$Gemeindename, " - ", location_first$Stadtkreis_Name),
      location_first$Gemeindename
    )

    distance_km <- round(location_first$distance_to_border_km, 1)

    # Helpers for Swiss-style formatting
    fmt    <- function(x) format(round(x), big.mark = "'", scientific = FALSE)
    signed <- function(x) paste0(if (x >= 0) "+" else "\u2212", fmt(abs(x)))

    # Population change (first to last year)
    pop_rows <- location_rows %>%
      distinct(Ausgangsjahr, .keep_all = TRUE) %>%
      filter(!is.na(Einwohner))
    pop_html <- if (nrow(pop_rows) >= 2) {
      p1 <- pop_rows$Einwohner[1]
      pl <- pop_rows$Einwohner[nrow(pop_rows)]
      chg <- pl - p1
      paste0(fmt(pl), " (", signed(chg), ", ", sprintf("%+.1f%%", (chg / p1) * 100),
             " since ", pop_rows$Ausgangsjahr[1], ")")
    } else fmt(pop_rows$Einwohner[1])

    # Median income change (first to last year with data)
    inc_rows <- location_rows %>% filter(!is.na(INCOME_VALUE))
    inc_html <- if (nrow(inc_rows) >= 2) {
      i1 <- inc_rows$INCOME_VALUE[1]
      il <- inc_rows$INCOME_VALUE[nrow(inc_rows)]
      chg <- il - i1
      paste0("CHF ", fmt(il), " (", signed(chg), ", ", sprintf("%+.1f%%", (chg / i1) * 100),
             " since ", inc_rows$Ausgangsjahr[1], ")")
    } else if (nrow(inc_rows) == 1) {
      paste0("CHF ", fmt(inc_rows$INCOME_VALUE[1]))
    } else "N/A"

    # Total burglaries across all years
    yr_min    <- min(location_rows$Ausgangsjahr)
    yr_max    <- max(location_rows$Ausgangsjahr)
    total_ebd <- sum(location_rows$Straftaten_total, na.rm = TRUE)

    # Merger summary (only for new_BFS municipalities)
    merger_info <- merger_mapping %>% filter(new_BFS_Nr == bfs_selected)
    merger_html <- if (nrow(merger_info) > 0) {
      merge_year <- merger_info$merge_year[1]
      old_names <- income_kt_raw %>%
        filter(BFS_NR %in% merger_info$old_BFS_Nr) %>%
        select(BFS_NR, GEBIET_NAME) %>%
        distinct() %>%
        mutate(GEBIET_NAME = gsub("\\bbis\\b", "until", GEBIET_NAME)) %>%
        pull(GEBIET_NAME)
      paste0("<br/><strong>Merger:</strong> Formed in ", merge_year,
             " from: ", paste(old_names, collapse = ", "))
    } else ""

    HTML(paste0(
      "<strong>Location:</strong> ", location_name, "<br/>",
      "<strong>BFS Number:</strong> ", bfs_selected, "<br/>",
      "<strong>Distance to Border:</strong> ", distance_km, " km<br/>",
      "<strong>Population (", yr_max, "):</strong> ", pop_html, "<br/>",
      "<strong>Median Income (", if (nrow(inc_rows) >= 1) max(inc_rows$Ausgangsjahr) else yr_max, "):</strong> ", inc_html, "<br/>",
      "<strong>Total Burglaries (", yr_min, "\u2013", yr_max, "):</strong> ", fmt(total_ebd),
      merger_html
    ))
  })

  # Population plot
  output$population_plot <- renderPlot({
    req(loc_info())
    loc <- loc_info()
    plot_population(ebd_viz, loc$municipality, loc$stadtkreis)
  }, height = function() {
    h <- session$clientData$output_population_plot_height
    if (is.null(h) || h == 0) 300L else h
  })

  # Income plot (uses merger visualization for merged municipalities)
  output$income_plot <- renderPlot({
    req(loc_info())
    loc <- loc_info()
    if (loc$is_merged && is.null(loc$stadtkreis)) {
      plot_income_merged(income_kt_viz, income_kt_raw, merger_mapping, loc$municipality)
    } else {
      plot_income(income_kt_viz, income_st_viz, loc$municipality, loc$stadtkreis)
    }
  }, height = function() {
    h <- session$clientData$output_income_plot_height
    if (is.null(h) || h == 0) 300L else h
  })

  # Burglary lines plot (all three Tatbestand types)
  output$burglary_plot <- renderPlot({
    req(loc_info())
    loc <- loc_info()
    plot_burglary_lines(ebd_viz, loc$municipality, loc$stadtkreis)
  }, height = function() {
    h <- session$clientData$output_burglary_plot_height
    if (is.null(h) || h == 0) 300L else h
  })

  # Burglary rate plot (per 1,000 inhabitants)
  output$burglary_rate_plot <- renderPlot({
    req(loc_info())
    loc <- loc_info()
    plot_ebd_rate(ebd_viz, loc$municipality, loc$stadtkreis)
  }, height = function() {
    h <- session$clientData$output_burglary_rate_plot_height
    if (is.null(h) || h == 0) 300L else h
  })

  # Render leaflet map
  output$map <- renderLeaflet({
    # Base map that doesn't change
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 8.65, lat = 47.44, zoom = 10)
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
                        "EBD_pop_ratio" = "Burglary Rate (per 1,000 Inhabitants)",
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

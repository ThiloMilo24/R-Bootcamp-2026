library(readr)
library(dplyr)
library(sf)
library(ggplot2)

### Data Processing

# Burglary data canton ZH
ebd <- read.csv("Data/KTZH_EBD.csv")

# Remove unnecessary records
ebd <- ebd %>% 
  filter(Tatbestand == 'Einbrüche insgesamt', 
         Gemeindename != "unbekannt ZH",
         Stadtkreis_Name != "unbekannt") %>% 
  select(-c(Gesetz_Nummer, Gesetz_Abk))

# Income data on municipality level
income_kt <- read.csv("Data/KTZH_Income_median.csv")

# Remove non-municipality and city data and unused columns
income_kt <- income_kt %>% 
  filter(!BFS_NR %in% c(0, 261)) %>% 
  select(BFS_NR, GEBIET_NAME, INDIKATOR_JAHR, INDIKATOR_VALUE) %>% 
  rename(INCOME_VALUE = INDIKATOR_VALUE)

# Income data on city district level
income_st <- read.csv("Data/STZH_Income_median.csv")

income_st <- income_st %>% 
  filter(SteuerTarifLang == "Grundtarif") %>% 
  select(-c("KreisSort", 
            "KreisCd", 
            "SteuerTarifSort", 
            "SteuerTarifCd",
            "SteuerTarifLang")) %>% 
  mutate(INCOME_VALUE = SteuerEinkommen_p50 * 1000)

### Missing Values

# EBD dataset - check data availability by municipality
ebd_years <- unique(ebd$Ausgangsjahr)
ebd_year_range <- range(ebd_years)

ebd_coverage <- ebd %>%
  group_by(Gemeinde_BFS_Nr, Gemeindename) %>%
  summarise(
    min_year = min(Ausgangsjahr),
    max_year = max(Ausgangsjahr),
    n_years = n_distinct(Ausgangsjahr),
    missing_years = max(ebd_years) - min(ebd_years) + 1 - n_years,
    dataset = "Burglary",
    .groups = 'drop'
  )

incomplete_ebd <- ebd_coverage %>%
  filter(min_year > min(ebd_years) | max_year < max(ebd_years) | missing_years > 0)

# Income_st dataset - check data availability
income_st_years <- unique(income_st$StichtagDatJahr)
income_st_year_range <- range(income_st_years)

income_st_coverage <- income_st %>%
  group_by(KreisLang) %>%
  summarise(
    min_year = min(StichtagDatJahr),
    max_year = max(StichtagDatJahr),
    n_years = n_distinct(StichtagDatJahr),
    missing_years = max(income_st_years) - min(income_st_years) + 1 - n_years,
    dataset = "Income (City Districts)",
    .groups = 'drop'
  ) %>%
  rename(Gemeindename = KreisLang) %>%
  mutate(Gemeinde_BFS_Nr = NA_integer_)

# Income_kt dataset - check data availability
income_kt_years <- unique(income_kt$INDIKATOR_JAHR)
income_kt_year_range <- range(income_kt_years)

income_kt_coverage <- income_kt %>%
  group_by(BFS_NR, GEBIET_NAME) %>%
  summarise(
    min_year = min(INDIKATOR_JAHR),
    max_year = max(INDIKATOR_JAHR),
    n_years = n_distinct(INDIKATOR_JAHR),
    missing_years = max(income_kt_years) - min(income_kt_years) + 1 - n_years,
    dataset = "Income (Canton)",
    .groups = 'drop'
  ) %>%
  rename(Gemeinde_BFS_Nr = BFS_NR, Gemeindename = GEBIET_NAME)

# Combine all coverage data
all_coverage <- bind_rows(
  ebd_coverage %>% select(Gemeinde_BFS_Nr, Gemeindename, n_years, min_year, max_year, missing_years, dataset),
  income_st_coverage %>% select(Gemeinde_BFS_Nr, Gemeindename, n_years, min_year, max_year, missing_years, dataset),
  income_kt_coverage %>% select(Gemeinde_BFS_Nr, Gemeindename, n_years, min_year, max_year, missing_years, dataset)
)

missing_coverage <- all_coverage %>%
  filter(missing_years > 0) %>%
  arrange(dataset, n_years) %>%
  select(dataset, Gemeindename, Gemeinde_BFS_Nr, n_years, missing_years, start_year = min_year, end_year = max_year)

### Handling Missing Values

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

### Merge Data Sets

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
ktzh_gpkg <- st_read("Data/KTZH_Gemeindegrenzen_OGD.gpkg", 
                             layer = "UP_GEMEINDEN_SEEN_F")

# Remove non-municipality and city geoms
ktzh_geom <- ktzh_gpkg %>% 
  filter(!ktzh_gpkg$BFS %in% c(0, 261), ktzh_gpkg$ART_CODE == 1) %>% 
  select(BFS, geom)

# Read .gpkg of swiss borders (swisstopo)
swiss_borders <- st_read("Data/CH_Borders_swisstopo.gpkg", 
                         layer = "tlm_landesgebiet")

swiss_borders <- swiss_borders %>% 
  filter(swiss_borders$icc == "CH")

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

# Join geoms of the municipalities
data <- data %>% 
  left_join(ktzh_geom, by = c("Gemeinde_BFS_Nr" = "BFS"))

# Convert the city district .gpkg CURVEPOLYGON TO MULTIPOLYGON using gdal_utils
gdal_utils(
  util = "vectortranslate",
  source = "Data/STZH_Stadtkreise_OGD.gpkg",
  destination = "Data/STZH_lin.gpkg",
  options = c("-nlt", "MULTIPOLYGON", 
              "-lco", "GEOMETRY_NAME=geom",
              "-overwrite")
)

# Read the converted file
stzh_gpkg <- st_read("Data/STZH_lin.gpkg")

stzh_geom <- stzh_gpkg %>% 
  select(kname, geom) %>% 
  st_transform(st_crs(swiss_borders)) %>% 
  mutate(
    distance_to_border = st_distance(st_centroid(.), swiss_border_line)[,1],
    distance_to_border_km = as.numeric(distance_to_border) / 1000
  )

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

### Plotting

# Create the outer boundary of Zurich city
city_outline <- data %>%
  filter(!is.na(Stadtkreis_BFS_Nr)) %>%
  st_union()  # Dissolves all districts into one polygon

# Get bounding box of municipalities
bbox <- st_bbox(ktzh_geom)
padding <- 5000  # in map units (meters for Swiss CRS)

# Plot the distance to border
gmd_to_border <- data %>% 
  group_by(Gemeinde_BFS_Nr) %>% 
  summarise(across(everything(), first), .groups = "drop") %>% 
  st_as_sf()  # Convert back to sf object

ggplot() +
  geom_sf(data = swiss_borders, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = gmd_to_border, 
          aes(fill = distance_to_border_km), color = "white", linewidth = 0.3) +
  geom_sf(data = city_outline, 
          fill = NA, 
          color = "black", 
          size = 0.5) +
  scale_fill_viridis_c(name = "Distance to\nborder (km)") +
  coord_sf(xlim = c(bbox["xmin"] - padding, bbox["xmax"] + padding), 
           ylim = c(bbox["ymin"] - padding, bbox["ymax"] + padding)) +
  theme_minimal() +
  labs(title = "Distance of ZH Municipalities to Swiss Border")

# Plot the crime rate across whole data set
ebd_per_gmd <- data %>% 
  group_by(Gemeinde_BFS_Nr) %>% 
  mutate(Total_EBD = sum(Straftaten_total, na.rm = TRUE), 
         Einwohner_avg = mean(Einwohner, na.rm = TRUE),
         EBD_pop_ratio = Total_EBD / Einwohner_avg) %>% 
  summarise(across(c(geom, Einwohner_avg, Total_EBD, EBD_pop_ratio), first),
  .groups = "drop") %>% 
  st_as_sf()

ggplot() +
  geom_sf(data = swiss_borders, fill = NA, color = "black", linewidth = 1) +
  geom_sf(data = ebd_per_gmd, aes(fill = EBD_pop_ratio), color = "white", linewidth = 0.3) +
  geom_sf(data = city_outline, 
          fill = NA, 
          color = "black", 
          size = 0.5) +
  scale_fill_viridis_c(name = "EBD ratio per population") +
  coord_sf(xlim = c(bbox["xmin"] - padding, bbox["xmax"] + padding), 
           ylim = c(bbox["ymin"] - padding, bbox["ymax"] + padding)) +
  theme_minimal() +
  labs(title = "Number of Burglaries per Municipality by Population")
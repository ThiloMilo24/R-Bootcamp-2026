library(readr)
library(dplyr)
library(sf)
library(ggplot2)

### Data Preprocessing

# accidents <- read.csv("Data/KTZH_Accident.csv")
# 
# # Remove language specific columns to reduce file size
# accidents_short <- accidents %>% 
#   select(-ends_with(c("_de", "_fr", "_it")))
# 
# write.csv(accidents_short, "Data/KTZH_Accident_short.csv")
# 
# # Aggregate total trafic accident per municipality and year
# accidents_agg <- accidents_short %>% 
#   group_by(MunicipalityCode_Aktuell, AccidentYear) %>% 
#   summarise(AccidentCount = n(), .groups = "drop")

### Data Processing

# Burglary data canton ZH
ebd <- read.csv("Data/KTZH_EBD.csv")

# Remove unnecessary records
ebd <- ebd %>% 
  filter(Tatbestand == 'Einbrüche insgesamt', 
         Gemeindename != "unbekannt ZH",
         Stadtkreis_Name != "unbekannt") %>% 
  select(-c(Gesetz_Nummer, Gesetz_Abk))

# Income data municipality level
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

# # Create aggregated Zürich city data (one row per year)
# zh_city <- data %>% 
#   filter(Gemeindename == "Zürich") %>% 
#   group_by(Ausgangsjahr) %>% 
#   summarise(
#     Gemeindename = "Zürich",
#     Gemeinde_BFS_Nr = 261,
#     Tatbestand = "Einbrüche insgesamt",
#     Einwohner = sum(Einwohner, na.rm = TRUE),
#     Straftaten_total = sum(Straftaten_total, na.rm = TRUE),
#     INCOME_VALUE = mean(INCOME_VALUE, na.rm = TRUE),
#     .groups = "drop")
# 
# # Replace Zürich districts with city-level data
# data <- data %>% 
#   filter(Gemeindename != "Zürich") %>%  # Remove all Zürich district rows
#   bind_rows(zh_city)  # Add back aggregated city rows

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

# City outline for map
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
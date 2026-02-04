### EDA: Gemeinde-Analyse — Einzelne Timelines + Cluster
# Voraussetzung: Data Preparation.R wurde zuvor ausgeführt (Objekt "data" muss existieren)

library(dplyr)
library(sf)
library(ggplot2)
library(tidyr)

## 1. Timeline für ausgewählte Gemeinden

# Auswahl: grösste Gemeinden + grenznahe + auffällige Einbruchsraten
# Dynamische Auswahl basierend auf den Daten
gemeinde_stats <- data %>%
  st_drop_geometry() %>%
  group_by(Gemeinde_BFS_Nr, Gemeindename) %>%
  summarise(
    einwohner_mean = mean(Einwohner, na.rm = TRUE),
    haeufigkeit_mean = mean(Häufigkeitszahl, na.rm = TRUE),
    distance_to_border_km = first(distance_to_border_km),
    .groups = "drop"
  )

# Top 3 grösste Gemeinden
top_pop <- gemeinde_stats %>% slice_max(einwohner_mean, n = 3)
# Top 3 höchste Einbruchsrate
top_crime <- gemeinde_stats %>% slice_max(haeufigkeit_mean, n = 3)
# Top 3 grenznächste Gemeinden
top_border <- gemeinde_stats %>% slice_min(distance_to_border_km, n = 3)

# Zusammenführen (unique)
selected <- bind_rows(top_pop, top_crime, top_border) %>%
  distinct(Gemeinde_BFS_Nr, .keep_all = TRUE)

# Zeitverlauf für ausgewählte Gemeinden
selected_timeline <- data %>%
  st_drop_geometry() %>%
  filter(Gemeinde_BFS_Nr %in% selected$Gemeinde_BFS_Nr) %>%
  select(Ausgangsjahr, Gemeindename, Häufigkeitszahl, Einwohner)

ggplot(selected_timeline, aes(x = Ausgangsjahr, y = Häufigkeitszahl,
                              color = Gemeindename)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(selected_timeline$Ausgangsjahr),
                                  max(selected_timeline$Ausgangsjahr), 1)) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  labs(
    title = "Burglary Rate Over Time — Selected Municipalities",
    subtitle = "Largest, highest crime rate, and closest to border",
    x = "Year",
    y = "Burglary Rate (per 1'000 inhabitants)",
    color = "Municipality"
  )

# Facetted: jede Gemeinde einzeln
ggplot(selected_timeline, aes(x = Ausgangsjahr, y = Häufigkeitszahl)) +
  geom_line(linewidth = 1, color = "#2c7fb8") +
  geom_point(size = 2, color = "#2c7fb8") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
              color = "#d95f02", linewidth = 0.6) +
  facet_wrap(~ Gemeindename, scales = "free_y") +
  scale_x_continuous(breaks = seq(min(selected_timeline$Ausgangsjahr),
                                  max(selected_timeline$Ausgangsjahr), 2)) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Burglary Rate Trends per Municipality",
    subtitle = "Dashed line = linear trend",
    x = "Year",
    y = "Burglary Rate"
  )

## 2. Cluster-Analyse: Gemeindetypen nach Einkommen und Einbruchsrate

# Aggregierte Daten pro Gemeinde
cluster_data <- data %>%
  st_drop_geometry() %>%
  group_by(Gemeinde_BFS_Nr, Gemeindename) %>%
  summarise(
    income_median = median(INCOME_VALUE, na.rm = TRUE),
    haeufigkeit_mean = mean(Häufigkeitszahl, na.rm = TRUE),
    einwohner_mean = mean(Einwohner, na.rm = TRUE),
    distance_to_border_km = first(distance_to_border_km),
    .groups = "drop"
  ) %>%
  filter(!is.na(income_median), !is.na(haeufigkeit_mean))

# Gemeinden in 4 Quadranten einteilen (Median als Schwelle)
income_threshold <- median(cluster_data$income_median)
crime_threshold <- median(cluster_data$haeufigkeit_mean)

cluster_data <- cluster_data %>%
  mutate(cluster = case_when(
    income_median >= income_threshold & haeufigkeit_mean >= crime_threshold
      ~ "High Income / High Crime",
    income_median >= income_threshold & haeufigkeit_mean < crime_threshold
      ~ "High Income / Low Crime",
    income_median < income_threshold & haeufigkeit_mean >= crime_threshold
      ~ "Low Income / High Crime",
    income_median < income_threshold & haeufigkeit_mean < crime_threshold
      ~ "Low Income / Low Crime"
  ))

# Quadranten-Scatterplot
ggplot(cluster_data, aes(x = income_median, y = haeufigkeit_mean,
                         color = cluster)) +
  geom_point(aes(size = einwohner_mean), alpha = 0.6) +
  geom_vline(xintercept = income_threshold, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = crime_threshold, linetype = "dashed", color = "grey40") +
  geom_text(data = cluster_data %>%
              filter(haeufigkeit_mean > quantile(haeufigkeit_mean, 0.95) |
                     einwohner_mean > quantile(einwohner_mean, 0.95)),
            aes(label = Gemeindename),
            size = 3, nudge_y = 0.5, show.legend = FALSE) +
  scale_color_manual(values = c(
    "High Income / High Crime" = "#e41a1c",
    "High Income / Low Crime"  = "#4daf4a",
    "Low Income / High Crime"  = "#ff7f00",
    "Low Income / Low Crime"   = "#377eb8"
  )) +
  scale_size_continuous(name = "Population (avg.)", labels = scales::comma) +
  scale_x_continuous(labels = scales::comma_format(big.mark = "'")) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Municipality Clusters: Income vs. Burglary Rate",
    subtitle = "Dashed lines = median thresholds across all municipalities",
    x = "Median Income (CHF)",
    y = "Burglary Rate (per 1'000 inhabitants)",
    color = "Cluster"
  )

# Cluster-Zusammenfassung
cluster_summary <- cluster_data %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    income_avg = mean(income_median),
    crime_avg = mean(haeufigkeit_mean),
    border_dist_avg = mean(distance_to_border_km, na.rm = TRUE),
    .groups = "drop"
  )
print(cluster_summary)

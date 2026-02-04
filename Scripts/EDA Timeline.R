### EDA: Timeline — Einbruchsrate und Einkommen über die Zeit
# Voraussetzung: Data Preparation.R wurde zuvor ausgeführt (Objekt "data" muss existieren)

library(dplyr)
library(sf)
library(ggplot2)
library(tidyr)

## Timeline: Einbruchsrate und Einkommen über die Zeit (Gesamtkanton)

# Jährliche Aggregate über alle Gemeinden
timeline_data <- data %>%
  st_drop_geometry() %>%
  group_by(Ausgangsjahr) %>%
  summarise(
    burglary_rate = mean(Häufigkeitszahl, na.rm = TRUE),
    income = mean(INCOME_VALUE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(income))

# Normalisierung auf Index (Basisjahr = 100)
base_year <- min(timeline_data$Ausgangsjahr)
base_vals <- timeline_data %>% filter(Ausgangsjahr == base_year)

timeline_data <- timeline_data %>%
  mutate(
    burglary_index = burglary_rate / base_vals$burglary_rate * 100,
    income_index = income / base_vals$income * 100
  )

# Long-Format für ggplot
timeline_long <- timeline_data %>%
  select(Ausgangsjahr, burglary_index, income_index) %>%
  pivot_longer(cols = c(burglary_index, income_index),
               names_to = "variable",
               values_to = "index") %>%
  mutate(variable = recode(variable,
                           "burglary_index" = "Burglary Rate",
                           "income_index" = "Median Income"))

ggplot(timeline_long, aes(x = Ausgangsjahr, y = index,
                          color = variable, linetype = variable)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  scale_color_manual(values = c("Burglary Rate" = "#d95f02",
                                "Median Income" = "#2c7fb8")) +
  scale_x_continuous(breaks = seq(base_year, max(timeline_data$Ausgangsjahr), 1)) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Burglary Rate and Median Income Over Time",
    subtitle = paste0("Canton Zurich, indexed (", base_year, " = 100)"),
    x = "Year",
    y = "Index",
    color = NULL,
    linetype = NULL
  )

## Separater Timeline-Chart: Einkommen

ggplot(timeline_data, aes(x = Ausgangsjahr, y = income)) +
  geom_line(linewidth = 1.2, color = "#2c7fb8") +
  geom_point(size = 2.5, color = "#2c7fb8") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
              color = "#2c7fb8", linewidth = 0.6) +
  scale_x_continuous(breaks = seq(base_year, max(timeline_data$Ausgangsjahr), 1)) +
  scale_y_continuous(labels = scales::comma_format(big.mark = "'")) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Median Income Over Time",
    subtitle = "Canton Zurich, mean across all municipalities per year",
    x = "Year",
    y = "Median Income (CHF)"
  )

## Separater Timeline-Chart: Einbruchsrate

ggplot(timeline_data, aes(x = Ausgangsjahr, y = burglary_rate)) +
  geom_line(linewidth = 1.2, color = "#d95f02") +
  geom_point(size = 2.5, color = "#d95f02") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
              color = "#d95f02", linewidth = 0.6) +
  scale_x_continuous(breaks = seq(base_year, max(timeline_data$Ausgangsjahr), 1)) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Burglary Rate Over Time",
    subtitle = "Canton Zurich, mean across all municipalities per year",
    x = "Year",
    y = "Burglary Rate (per 1'000 inhabitants)"
  )

## Einbruchsrate über Zeit, gruppiert nach Grenzdistanz

# Gemeinden in Distanz-Gruppen einteilen
border_timeline <- data %>%
  st_drop_geometry() %>%
  filter(!is.na(distance_to_border_km), !is.na(Häufigkeitszahl)) %>%
  mutate(border_group = cut(distance_to_border_km,
                            breaks = c(0, 10, 20, Inf),
                            labels = c("< 10 km", "10–20 km", "> 20 km"))) %>%
  group_by(Ausgangsjahr, border_group) %>%
  summarise(
    burglary_rate = mean(Häufigkeitszahl, na.rm = TRUE),
    n_municipalities = n_distinct(Gemeinde_BFS_Nr),
    .groups = "drop"
  )

ggplot(border_timeline, aes(x = Ausgangsjahr, y = burglary_rate,
                            color = border_group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("< 10 km" = "#e41a1c",
                                "10–20 km" = "#ff7f00",
                                "> 20 km" = "#4daf4a")) +
  scale_x_continuous(breaks = seq(base_year, max(border_timeline$Ausgangsjahr), 1)) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Burglary Rate Over Time by Distance to National Border",
    subtitle = "Canton Zurich, municipalities grouped by centroid distance to border",
    x = "Year",
    y = "Burglary Rate (per 1'000 inhabitants)",
    color = "Distance to Border"
  )

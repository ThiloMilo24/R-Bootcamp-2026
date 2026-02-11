### EDA: Deskriptive Statistik — Überblick über den Datensatz
# Prerequisite: Data Preparation.R must have been run (object "data" must exist)

library(dplyr)
library(sf)
library(ggplot2)
library(tidyr)
library(scales)

# ---------------------------------------------------------------------------
# 1. Dataset dimensions and coverage
# ---------------------------------------------------------------------------
desc_data <- data %>% st_drop_geometry()

n_obs <- nrow(desc_data)
n_municipalities <- n_distinct(desc_data$Gemeinde_BFS_Nr)
year_range <- range(desc_data$Ausgangsjahr)
n_years <- n_distinct(desc_data$Ausgangsjahr)

cat("Observations:", n_obs, "\n")
cat("Municipalities / City districts:", n_municipalities, "\n")
cat("Years:", year_range[1], "–", year_range[2], "(", n_years, "years)\n")

# ---------------------------------------------------------------------------
# 2. Summary statistics for key variables
# ---------------------------------------------------------------------------
summary_stats <- desc_data %>%
  summarise(
    across(
      c(Straftaten_total, Einwohner, Häufigkeitszahl, INCOME_VALUE, distance_to_border_km),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        min = ~min(.x, na.rm = TRUE),
        max = ~max(.x, na.rm = TRUE),
        n_missing = ~sum(is.na(.x))
      ),
      .names = "{.col}__{.fn}"
    )
  ) %>%
  pivot_longer(everything(),
               names_to = c("variable", "stat"),
               names_sep = "__") %>%
  pivot_wider(names_from = stat, values_from = value)

print(summary_stats)

# ---------------------------------------------------------------------------
# 3. Distribution of burglary counts
# ---------------------------------------------------------------------------
ggplot(desc_data, aes(x = Straftaten_total)) +
  geom_histogram(bins = 50, fill = "#2c7fb8", alpha = 0.7, color = "white") +
  scale_x_continuous(labels = comma) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Distribution of Burglary Counts",
    subtitle = "Per municipality-year observation",
    x = "Number of Burglaries",
    y = "Frequency"
  )

# ---------------------------------------------------------------------------
# 4. Distribution of income
# ---------------------------------------------------------------------------
ggplot(desc_data %>% filter(!is.na(INCOME_VALUE)),
       aes(x = INCOME_VALUE)) +
  geom_histogram(bins = 40, fill = "#2c7fb8", alpha = 0.7, color = "white") +
  scale_x_continuous(labels = comma_format(big.mark = "'")) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Distribution of Median Income",
    subtitle = "Per municipality-year observation",
    x = "Median Income (CHF)",
    y = "Frequency"
  )

# ---------------------------------------------------------------------------
# 5. Distribution of border distance
# ---------------------------------------------------------------------------
ggplot(desc_data %>% filter(!is.na(distance_to_border_km)),
       aes(x = distance_to_border_km)) +
  geom_histogram(bins = 30, fill = "#2c7fb8", alpha = 0.7, color = "white") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Distribution of Distance to National Border",
    subtitle = "Per municipality (centroid distance)",
    x = "Distance to Border (km)",
    y = "Frequency"
  )

# ---------------------------------------------------------------------------
# 6. Population size distribution (log scale)
# ---------------------------------------------------------------------------
ggplot(desc_data, aes(x = Einwohner)) +
  geom_histogram(bins = 50, fill = "#2c7fb8", alpha = 0.7, color = "white") +
  scale_x_log10(labels = comma) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Distribution of Municipality Population",
    subtitle = "Log scale — municipalities vary enormously in size",
    x = "Population (log scale)",
    y = "Frequency"
  )

# ---------------------------------------------------------------------------
# 7. Correlation matrix of key variables
# ---------------------------------------------------------------------------
cor_data <- desc_data %>%
  select(Straftaten_total, Einwohner, Häufigkeitszahl,
         INCOME_VALUE, distance_to_border_km) %>%
  filter(complete.cases(.))

cor_matrix <- cor(cor_data)
print(round(cor_matrix, 3))

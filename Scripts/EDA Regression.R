### Explorative Datenanalyse
# Voraussetzung: Data Preparation.R wurde zuvor ausgeführt (Objekt "data" muss existieren)

library(dplyr)
library(sf)
library(ggplot2)

## H1: Zusammenhang Einkommen und Einbruchsrate

# Aggregation pro Gemeinde/Stadtkreis über alle Jahre
h1_data <- data %>%
  st_drop_geometry() %>%
  group_by(Gemeinde_BFS_Nr, Gemeindename) %>%
  summarise(
    income_median = median(INCOME_VALUE, na.rm = TRUE),
    haeufigkeit_mean = mean(Häufigkeitszahl, na.rm = TRUE),
    einwohner_mean = mean(Einwohner, na.rm = TRUE),
    distance_to_border_km = first(distance_to_border_km),
    n_years = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(income_median), !is.na(haeufigkeit_mean))

# Lineares Modell
lm_h1 <- lm(haeufigkeit_mean ~ income_median, data = h1_data)

# R² und p-Wert für Annotation
r2 <- summary(lm_h1)$r.squared
p_val <- summary(lm_h1)$coefficients[2, 4]
label_text <- paste0("R² = ", round(r2, 3),
                     "\np = ", formatC(p_val, format = "e", digits = 2))

# Scatterplot mit Regressionsgerade
ggplot(h1_data, aes(x = income_median, y = haeufigkeit_mean)) +
  geom_point(aes(size = einwohner_mean), alpha = 0.5, color = "#2c7fb8") +
  geom_smooth(method = "lm", se = TRUE, color = "#d95f02", linewidth = 1) +
  scale_x_continuous(labels = scales::comma_format(big.mark = "'")) +
  scale_size_continuous(name = "Population (avg.)", labels = scales::comma) +
  annotate("text", x = Inf, y = Inf, label = label_text,
           hjust = 1.1, vjust = 1.5, size = 4, fontface = "italic") +
  theme_minimal(base_size = 13) +
  labs(
    title = "H1: Median Income vs. Burglary Rate",
    subtitle = "Per municipality/city district, aggregated across all years",
    x = "Median Income (CHF)",
    y = "Burglary Rate (per 1'000 inhabitants)"
  )

## Multiple Regression: Einkommen + Grenzdistanz

# Daten filtern (Grenzdistanz muss vorhanden sein)
h1_multi_data <- h1_data %>%
  filter(!is.na(distance_to_border_km))

# Multiples lineares Modell
lm_multi <- lm(haeufigkeit_mean ~ income_median + distance_to_border_km,
               data = h1_multi_data)
summary(lm_multi)

# Koeffizienten für Annotation
s <- summary(lm_multi)
r2_multi <- s$r.squared
adj_r2 <- s$adj.r.squared
p_income <- s$coefficients["income_median", 4]
p_border <- s$coefficients["distance_to_border_km", 4]
f_stat <- s$fstatistic
p_model <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)

# Scatterplot: Einbruchsrate vs. Grenzdistanz (kontrolliert für Einkommen via Farbe)
ggplot(h1_multi_data, aes(x = distance_to_border_km, y = haeufigkeit_mean)) +
  geom_point(aes(color = income_median, size = einwohner_mean), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "#d95f02", linewidth = 1) +
  scale_color_viridis_c(name = "Median Income\n(CHF)",
                        labels = scales::comma_format(big.mark = "'")) +
  scale_size_continuous(name = "Population (avg.)", labels = scales::comma) +
  annotate("text", x = Inf, y = Inf,
           label = paste0("Multiple Model:\n",
                          "R² = ", round(r2_multi, 3),
                          " (adj. ", round(adj_r2, 3), ")\n",
                          "p(Income) = ", formatC(p_income, format = "e", digits = 2), "\n",
                          "p(Border dist.) = ", formatC(p_border, format = "e", digits = 2)),
           hjust = 1.1, vjust = 1.3, size = 3.5, fontface = "italic") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Burglary Rate vs. Distance to National Border",
    subtitle = "Color = Median Income | Multiple Regression: Rate ~ Income + Border Distance",
    x = "Distance to National Border (km)",
    y = "Burglary Rate (per 1'000 inhabitants)"
  )

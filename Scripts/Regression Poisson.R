### Poisson Regression: Burglary Counts
# Prerequisite: Data Preparation.R must have been run (object "data" must exist)
#
# Why Poisson?
# Burglary counts (Straftaten_total) are non-negative integers — classic count data.
# A Poisson model is the natural starting point for modelling counts. It assumes
# that the mean and variance of the outcome are equal (equidispersion).
# We use log(Einwohner) as an offset so that we effectively model the *rate*
# (burglaries per capita) rather than the raw count. This accounts for the fact
# that larger municipalities naturally have more burglaries simply because they
# have more people.

library(dplyr)
library(sf)
library(ggplot2)

# ---------------------------------------------------------------------------
# 1. Prepare panel data (one row = one municipality x one year)
# ---------------------------------------------------------------------------
model_data <- data %>%

  st_drop_geometry() %>%
  filter(
    !is.na(Straftaten_total),
    !is.na(INCOME_VALUE),
    !is.na(distance_to_border_km),
    Einwohner > 0
  ) %>%
  mutate(
    log_pop = log(Einwohner),
    income_10k = INCOME_VALUE / 10000,        # scale for interpretable coefficients
    year_centered = Ausgangsjahr - min(Ausgangsjahr)
  )

# ---------------------------------------------------------------------------
# 2. Fit Poisson Models
# ---------------------------------------------------------------------------

# Model 1: Income only
poisson_m1 <- glm(
  Straftaten_total ~ income_10k + offset(log_pop),
  family = poisson(link = "log"),
  data = model_data
)

# Model 2: Border distance only
poisson_m2 <- glm(
  Straftaten_total ~ distance_to_border_km + offset(log_pop),
  family = poisson(link = "log"),
  data = model_data
)

# Model 3: Income + Border distance
poisson_m3 <- glm(
  Straftaten_total ~ income_10k + distance_to_border_km + offset(log_pop),
  family = poisson(link = "log"),
  data = model_data
)

# Model 4: Full model with year trend
poisson_full <- glm(
  Straftaten_total ~ income_10k + distance_to_border_km + year_centered +
    offset(log_pop),
  family = poisson(link = "log"),
  data = model_data
)

# ---------------------------------------------------------------------------
# 3. Diagnostics: Check for Overdispersion
# ---------------------------------------------------------------------------
# The Poisson assumption is that Var(Y) = E(Y). If Var(Y) > E(Y), the data
# are overdispersed and standard errors from the Poisson model are too small,
# leading to falsely significant p-values.

calc_dispersion <- function(model) {
  pearson_resid <- residuals(model, type = "pearson")
  sum(pearson_resid^2) / model$df.residual
}

dispersion_full <- calc_dispersion(poisson_full)

cat("Poisson Full Model — Dispersion statistic:", round(dispersion_full, 2), "\n")
cat("(Values >> 1 indicate overdispersion; the Poisson assumption is violated.)\n\n")

# ---------------------------------------------------------------------------
# 4. Summary output
# ---------------------------------------------------------------------------
cat("=== Poisson Model: Income only ===\n")
print(summary(poisson_m1))

cat("\n=== Poisson Model: Border distance only ===\n")
print(summary(poisson_m2))

cat("\n=== Poisson Model: Income + Border distance ===\n")
print(summary(poisson_m3))

cat("\n=== Poisson Full Model: Income + Border + Year ===\n")
print(summary(poisson_full))

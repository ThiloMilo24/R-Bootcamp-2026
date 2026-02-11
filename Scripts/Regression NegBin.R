### Negative Binomial Regression: Burglary Counts
# Prerequisite: Data Preparation.R must have been run (object "data" must exist)
#
# ---------------------------------------------------------------------------
# WHY NEGATIVE BINOMIAL?
# ---------------------------------------------------------------------------
# The Poisson model assumes that the variance equals the mean (equidispersion).
# In practice, count data — especially crime data — almost always show
# *overdispersion*: the variance is much larger than the mean. This happens
# because unobserved factors (e.g. local policing, socioeconomic differences
# not captured in our variables) create extra variability between municipalities.
#
# The Negative Binomial (NB) model adds an extra parameter (theta) that
# captures this excess variability. When theta is large, the NB converges
# to the Poisson; when theta is small, there is substantial overdispersion.
#
# ---------------------------------------------------------------------------
# WHAT IS AN OFFSET VARIABLE?
# ---------------------------------------------------------------------------
# We want to model burglary *rates* (per capita), not raw counts. But our
# outcome variable is a count (Straftaten_total). The offset solves this:
#
#   log(count) = beta * X + log(population)
#
# Rearranging:
#
#   log(count / population) = beta * X
#
# So by including log(Einwohner) as an offset, we force the model to account
# for population size. A municipality with 50,000 people is expected to have
# more burglaries than one with 5,000 — not because it's more dangerous,
# but simply because it's bigger. The offset corrects for this.
#
# ---------------------------------------------------------------------------
# WHAT ARE COVARIATES?
# ---------------------------------------------------------------------------
# Covariates are the predictor variables we include in the model to explain
# variation in burglary counts:
#
# - income_10k: Median income (in 10k CHF). Tests H1: do wealthier
#   municipalities attract more burglaries?
#
# - distance_to_border_km: Distance from municipality centroid to the
#   nearest Swiss national border (km). Tests H2: are border-proximate
#   municipalities more affected?
#
# - year_centered: Year (centered at the first observation year). Controls
#   for the canton-wide temporal trend (declining burglaries over time).
#
# - income_10k:distance_to_border_km (interaction): Tests H3: does the
#   effect of income on burglaries depend on how close a municipality is
#   to the border? For example, high income might attract more burglaries
#   *only* near the border.
# ---------------------------------------------------------------------------

library(dplyr)
library(sf)
library(MASS)       # for glm.nb()
library(ggplot2)

# ---------------------------------------------------------------------------
# 1. Prepare panel data
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
    income_10k = INCOME_VALUE / 10000,
    year_centered = Ausgangsjahr - min(Ausgangsjahr)
  )

# ---------------------------------------------------------------------------
# 2. Fit Negative Binomial Models
# ---------------------------------------------------------------------------

# Model 1: Income only (with population offset)
nb_m1 <- glm.nb(
  Straftaten_total ~ income_10k + offset(log_pop),
  data = model_data
)

# Model 2: Border distance only
nb_m2 <- glm.nb(
  Straftaten_total ~ distance_to_border_km + offset(log_pop),
  data = model_data
)

# Model 3: Income + Border distance (H3: joint effect)
nb_m3 <- glm.nb(
  Straftaten_total ~ income_10k + distance_to_border_km + offset(log_pop),
  data = model_data
)

# Model 4: Full model with year trend
nb_full <- glm.nb(
  Straftaten_total ~ income_10k + distance_to_border_km + year_centered +
    offset(log_pop),
  data = model_data
)

# Model 5: Full model with interaction term
nb_interaction <- glm.nb(
  Straftaten_total ~ income_10k * distance_to_border_km + year_centered +
    offset(log_pop),
  data = model_data
)

# ---------------------------------------------------------------------------
# 3. Model summaries
# ---------------------------------------------------------------------------
cat("=== NB Model 1: Income only ===\n")
print(summary(nb_m1))

cat("\n=== NB Model 2: Border distance only ===\n")
print(summary(nb_m2))

cat("\n=== NB Model 3: Income + Border distance ===\n")
print(summary(nb_m3))

cat("\n=== NB Model 4: Full (Income + Border + Year) ===\n")
print(summary(nb_full))

cat("\n=== NB Model 5: With Interaction ===\n")
print(summary(nb_interaction))

# ---------------------------------------------------------------------------
# 4. Interpret theta
# ---------------------------------------------------------------------------
cat("\n--- Theta (overdispersion parameter) ---\n")
cat("NB Full Model theta:", round(nb_full$theta, 3), "\n")
cat("  (Smaller theta = more overdispersion. If theta -> Inf, NB = Poisson.)\n")

# ---------------------------------------------------------------------------
# 5. Incidence Rate Ratios (IRR)
# ---------------------------------------------------------------------------
# Exponentiated coefficients give Incidence Rate Ratios:
# - IRR > 1: variable increases the burglary rate
# - IRR < 1: variable decreases the burglary rate
# - IRR = 1: no effect

cat("\n--- Incidence Rate Ratios (Full Model) ---\n")
irr <- exp(coef(nb_full))
irr_ci <- exp(confint(nb_full))
irr_table <- data.frame(
  IRR = round(irr, 4),
  CI_lower = round(irr_ci[, 1], 4),
  CI_upper = round(irr_ci[, 2], 4)
)
print(irr_table)

cat("\nInterpretation example:\n")
cat("  IRR for distance_to_border_km: Each additional km from the border\n")
cat("  multiplies the expected burglary rate by this factor.\n")
cat("  IRR < 1 means the rate *decreases* with distance from the border.\n")

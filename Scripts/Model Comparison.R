### Model Comparison: Poisson vs. Negative Binomial
# Prerequisite: Run Data Preparation.R, Regression Poisson.R, Regression NegBin.R first
#
# This script compares the Poisson and Negative Binomial models to determine
# which better fits the burglary count data.

library(dplyr)
library(sf)
library(MASS)
library(ggplot2)
library(tidyr)

# ---------------------------------------------------------------------------
# 1. Prepare data (same as in both regression scripts)
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
# 2. Fit all models
# ---------------------------------------------------------------------------
poisson_full <- glm(
  Straftaten_total ~ income_10k + distance_to_border_km + year_centered +
    offset(log_pop),
  family = poisson(link = "log"),
  data = model_data
)

nb_full <- glm.nb(
  Straftaten_total ~ income_10k + distance_to_border_km + year_centered +
    offset(log_pop),
  data = model_data
)

nb_interaction <- glm.nb(
  Straftaten_total ~ income_10k * distance_to_border_km + year_centered +
    offset(log_pop),
  data = model_data
)

# ---------------------------------------------------------------------------
# 3. Likelihood Ratio Test (Poisson vs. NB)
# ---------------------------------------------------------------------------
# The Poisson model is nested within the NB (NB reduces to Poisson when
# theta -> Inf). We can test whether the extra parameter theta significantly
# improves the fit.

lr_stat <- 2 * (logLik(nb_full) - logLik(poisson_full))
lr_pval <- pchisq(as.numeric(lr_stat), df = 1, lower.tail = FALSE)

cat("=== Likelihood Ratio Test: Poisson vs. Negative Binomial ===\n")
cat("  LR statistic:", round(as.numeric(lr_stat), 2), "\n")
cat("  p-value:", formatC(lr_pval, format = "e", digits = 3), "\n")
cat("  Conclusion:", ifelse(lr_pval < 0.05,
    "NB significantly better than Poisson — overdispersion is present.",
    "No significant improvement — Poisson is adequate."), "\n\n")

# ---------------------------------------------------------------------------
# 4. AIC Comparison
# ---------------------------------------------------------------------------
aic_comparison <- data.frame(
  Model = c("Poisson (full)", "Negative Binomial (full)", "NB with interaction"),
  AIC = round(c(AIC(poisson_full), AIC(nb_full), AIC(nb_interaction)), 1),
  LogLik = round(c(as.numeric(logLik(poisson_full)),
                    as.numeric(logLik(nb_full)),
                    as.numeric(logLik(nb_interaction))), 1)
)

cat("=== AIC Comparison ===\n")
print(aic_comparison)
cat("  Lower AIC = better fit.\n\n")

# ---------------------------------------------------------------------------
# 5. Coefficient Comparison Table
# ---------------------------------------------------------------------------
poisson_coefs <- summary(poisson_full)$coefficients
nb_coefs <- summary(nb_full)$coefficients

comparison_table <- data.frame(
  Variable = rownames(poisson_coefs),
  Poisson_Est = round(poisson_coefs[, "Estimate"], 4),
  Poisson_SE = round(poisson_coefs[, "Std. Error"], 4),
  Poisson_p = formatC(poisson_coefs[, "Pr(>|z|)"], format = "e", digits = 2),
  NB_Est = round(nb_coefs[, "Estimate"], 4),
  NB_SE = round(nb_coefs[, "Std. Error"], 4),
  NB_p = formatC(nb_coefs[, "Pr(>|z|)"], format = "e", digits = 2)
)

cat("=== Coefficient Comparison ===\n")
print(comparison_table, row.names = FALSE)
cat("\nNote: The Poisson model typically has *smaller* standard errors.\n")
cat("This is misleading — it overestimates precision because it ignores\n")
cat("overdispersion. The NB standard errors are more honest.\n\n")

# ---------------------------------------------------------------------------
# 6. Visual: Coefficient Comparison (Poisson vs. NB)
# ---------------------------------------------------------------------------
vars <- c("income_10k", "distance_to_border_km", "year_centered")
var_labels <- c("Income (10k CHF)", "Distance to Border (km)", "Year")

comp_data <- data.frame(
  Variable = rep(var_labels, 2),
  Model = rep(c("Poisson", "Negative Binomial"), each = 3),
  Estimate = c(poisson_coefs[vars, "Estimate"], nb_coefs[vars, "Estimate"]),
  SE = c(poisson_coefs[vars, "Std. Error"], nb_coefs[vars, "Std. Error"])
)

ggplot(comp_data, aes(x = Variable, y = Estimate, color = Model)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE),
                position = position_dodge(width = 0.5), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("Poisson" = "#d95f02", "Negative Binomial" = "#2c7fb8")) +
  coord_flip() +
  theme_minimal(base_size = 13) +
  labs(
    title = "Coefficient Estimates: Poisson vs. Negative Binomial",
    subtitle = "Error bars = 95% confidence intervals",
    x = NULL,
    y = "Coefficient Estimate",
    color = NULL
  )

# ---------------------------------------------------------------------------
# 7. Visual: Observed vs. Predicted
# ---------------------------------------------------------------------------
model_data <- model_data %>%
  mutate(
    pred_poisson = predict(poisson_full, type = "response"),
    pred_nb = predict(nb_full, type = "response")
  )

pred_long <- model_data %>%
  select(Straftaten_total, pred_poisson, pred_nb) %>%
  pivot_longer(cols = c(pred_poisson, pred_nb),
               names_to = "model",
               values_to = "predicted") %>%
  mutate(model = recode(model,
                        "pred_poisson" = "Poisson",
                        "pred_nb" = "Negative Binomial"))

ggplot(pred_long, aes(x = predicted, y = Straftaten_total, color = model)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ model) +
  scale_color_manual(values = c("Poisson" = "#d95f02", "Negative Binomial" = "#2c7fb8")) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Observed vs. Predicted Burglary Counts",
    subtitle = "Dashed line = perfect prediction (log-log scale)",
    x = "Predicted Count",
    y = "Observed Count"
  ) +
  theme(legend.position = "none")

# ---------------------------------------------------------------------------
# 8. Rootogram: Distribution of Residuals
# ---------------------------------------------------------------------------
# Pearson residuals: (observed - expected) / sqrt(variance)
# For a well-fitting model, these should be roughly standard normal.

model_data <- model_data %>%
  mutate(
    resid_poisson = residuals(poisson_full, type = "pearson"),
    resid_nb = residuals(nb_full, type = "pearson")
  )

resid_long <- model_data %>%
  select(resid_poisson, resid_nb) %>%
  pivot_longer(everything(),
               names_to = "model",
               values_to = "residual") %>%
  mutate(model = recode(model,
                        "resid_poisson" = "Poisson",
                        "resid_nb" = "Negative Binomial"))

ggplot(resid_long, aes(x = residual)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = "#2c7fb8", alpha = 0.7) +
  stat_function(fun = dnorm, color = "#d95f02", linewidth = 1) +
  facet_wrap(~ model, scales = "free_x") +
  coord_cartesian(xlim = c(-5, 15)) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Distribution of Pearson Residuals",
    subtitle = "Orange curve = standard normal (expected for a well-fitting model)",
    x = "Pearson Residual",
    y = "Density"
  )

# ---------------------------------------------------------------------------
# 9. Summary Statistics
# ---------------------------------------------------------------------------
cat("=== Residual Summary ===\n")
cat("Poisson — Mean:", round(mean(model_data$resid_poisson), 3),
    " SD:", round(sd(model_data$resid_poisson), 3), "\n")
cat("NB      — Mean:", round(mean(model_data$resid_nb), 3),
    " SD:", round(sd(model_data$resid_nb), 3), "\n")
cat("(For a well-fitting model: mean ≈ 0, SD ≈ 1)\n")

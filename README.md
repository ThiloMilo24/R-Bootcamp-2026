# R-Bootcamp-2026

**Income, Crime, and Borders in Canton Zurich**

HSLU R Bootcamp group assignment by Thilo Holstein and Hans Josef Thalathara.

This project investigates whether anecdotal observations from Basel-Landschaft -- that burglaries concentrate in wealthier, border-proximate municipalities -- hold in the Canton of Zurich. We combine burglary statistics, income data, and geographic boundary data at the municipal level and test these assumptions using exploratory visualisation and formal count regression models (Poisson and Negative Binomial).

## Research Question

> How does income level relate to burglary rates across municipalities in Canton Zurich, and does proximity to the national border act as an independent predictor of crime?

## Hypotheses

- **H1 -- Income and Burglaries:** Municipalities with higher median income exhibit higher burglary rates, as wealthier areas may present more attractive targets.
- **H2 -- Border Proximity and Burglaries:** Municipalities closer to national borders experience higher burglary rates due to proximity to cross-border escape routes.
- **H3 -- Combined Effect:** Income and border proximity jointly predict burglary rates better than either variable alone.

## Data Sources

| Dataset | Source | Format |
|---|---|---|
| Burglary statistics (2009--2024) | Canton Zurich (opendata.swiss) | CSV |
| Median income by municipality (1999--2022) | Canton Zurich (opendata.swiss) | CSV |
| Median income by city district (2009--2022) | City of Zurich (data.stadt-zuerich.ch) | CSV |
| Municipality boundaries | Canton Zurich (opendata.swiss) | GeoPackage |
| Swiss national borders | swisstopo | GeoPackage |

## Project Structure

```
R-Bootcamp-2026/
├── Report.rmd                          # Main report (R Markdown)
├── Report.pdf                          # Rendered PDF output
├── Scripts/
│   ├── Data Preparation.R              # Data loading, cleaning, merging, merger reconciliation
│   ├── EDA Descriptive.R               # Descriptive statistics and distributions
│   ├── EDA Timeline.R                  # Temporal trends (income and burglary over time)
│   ├── EDA Regression.R                # Scatterplots with regression lines (H1, H2)
│   ├── EDA Municipalities.R            # Municipality timelines and income-crime clusters
│   ├── Regression Poisson.R            # Poisson count regression models
│   ├── Regression NegBin.R             # Negative Binomial models + IRR
│   └── Model Comparison.R             # Poisson vs. NB comparison (LR test, AIC, residuals)
├── App/
│   └── app.R                           # Interactive Shiny web application (Chapter of Choice)
├── Data/                               # All source datasets (CSV + GeoPackage)
└── R-Bootcamp-2026.Rproj              # RStudio project file
```

## How to Reproduce

1. Open `R-Bootcamp-2026.Rproj` in RStudio.
2. Ensure the required packages are installed: `readr`, `dplyr`, `MASS`, `sf`, `ggplot2`, `tidyr`, `scales`, `gridExtra`, `kableExtra`.
3. Knit `Report.rmd` to PDF (uses `xelatex` engine).
4. Individual analysis scripts in `Scripts/` can be run standalone after running `Data Preparation.R`.

## Key Findings

- **H1 rejected (direction reversed):** Wealthier municipalities show *lower* burglary rates.
- **H2 not supported:** Border distance is not statistically significant in the NB model (p > 0.05), despite a visually compelling spatial gradient in the EDA.
- **H3 partially supported:** The income-border interaction is statistically significant but adds only modest explanatory power.
- **Strongest predictor:** Year -- burglary rates have declined substantially across all municipalities since 2009.

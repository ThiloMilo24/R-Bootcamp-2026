library(readr)
library(dplyr)
library(ggplot2)

# Burglary data canton ZH
ebd <- read.csv("Data/KTZH_EBD.csv")

# Remove unnecessary records
ebd <- ebd %>%
  filter(Gemeindename != "unbekannt ZH",
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

# Save raw income_kt before merger handling (needed for merger visualization)
income_kt_raw <- income_kt

# Handle missing income_kt values caused by municipal mergers
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
    income_kt %>% select(BFS_NR, GEBIET_NAME) %>% distinct(),
    by = "BFS_NR"
  )

# Remove old pre-merger municipalities and add synthetic rows
income_kt <- income_kt %>%
  filter(!BFS_NR %in% merger_mapping$old_BFS_Nr) %>%
  bind_rows(income_old_bfs_all) %>%
  arrange(BFS_NR, INDIKATOR_JAHR)

### Visualizations

# Line plot showing all three Tatbestand types over time for a selected municipality.
# "Einbrüche insgesamt" (total) is highlighted; the two sub-types are dimmed.
# For Zürich city districts pass the district name via `stadtkreis`.
# Examples:
#   plot_burglary_lines(ebd, "Adliswil")
#   plot_burglary_lines(ebd, "Zürich", stadtkreis = "Kreis 4")
plot_burglary_lines <- function(data, municipality, stadtkreis = NULL) {

  df <- data %>%
    filter(Gemeindename == municipality)

  if (!is.null(stadtkreis)) {
    df <- df %>% filter(Stadtkreis_Name == stadtkreis)
  }

  title_label <- if (!is.null(stadtkreis)) {
    paste0(municipality, " - ", stadtkreis)
  } else {
    municipality
  }

  df <- df %>%
    mutate(Tatbestand = factor(Tatbestand, levels = c(
      "Einbrüche insgesamt",
      "Einbruchdiebstahl",
      "Einschleichdiebstahl"
    )))

  ggplot(df, aes(Ausgangsjahr, Straftaten_total, colour = Tatbestand, linewidth = Tatbestand)) +
    geom_line() +
    geom_point(size = 2) +
    scale_color_manual(values = c(
      "Einbrüche insgesamt"    = "#D6604D",
      "Einbruchdiebstahl"      = "#4393C3",
      "Einschleichdiebstahl"   = "#4DAC26"
    )) +
    scale_linewidth_manual(values = c(
      "Einbrüche insgesamt"    = 1.5,
      "Einbruchdiebstahl"      = 0.8,
      "Einschleichdiebstahl"   = 0.8
    )) +
    guides(
      colour = guide_legend(override.aes = list(linewidth = c(1.5, 0.8, 0.8))),
      linewidth = "none"
    ) +
    scale_x_continuous(breaks = unique(df$Ausgangsjahr)) +
    labs(
      title = paste("Burglary Trends:", title_label),
      x = "",
      y = "Number of Cases",
      colour = "Type"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_burglary_lines(ebd, "Adliswil")

# Line plot showing median income over time for a selected municipality.
# For Zürich city districts, pass the district name via `stadtkreis` — income_st
# is used automatically in that case.
# Examples:
#   plot_income(income_kt, income_st, "Adliswil")
#   plot_income(income_kt, income_st, "Zürich", stadtkreis = "Kreis 4")
plot_income <- function(kt_data, st_data, municipality, stadtkreis = NULL) {

  if (!is.null(stadtkreis)) {
    df <- st_data %>%
      filter(KreisLang == stadtkreis) %>%
      rename(INDIKATOR_JAHR = StichtagDatJahr)
    title_label <- paste0(municipality, " - ", stadtkreis)
  } else {
    df <- kt_data %>%
      filter(GEBIET_NAME == municipality)
    title_label <- municipality
  }

  ggplot(df, aes(x = INDIKATOR_JAHR, y = INCOME_VALUE)) +
    geom_line(colour = "#4393C3", linewidth = 1.5) +
    geom_point(colour = "#4393C3", size = 2) +
    scale_x_continuous(breaks = unique(df$INDIKATOR_JAHR)) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = paste("Median Income:", title_label),
      x = "",
      y = "Median Income (CHF)"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Line plot showing how pre-merger municipality incomes flow into the new merged
# municipality. Old municipalities shown as thin lines up to the merge year,
# new municipality as a thick line from the merge year onwards.
# Only valid for municipalities created by a merger — use plot_income() otherwise.
plot_income_merged <- function(kt_data, kt_raw, mergers, municipality) {

  new_bfs <- kt_data %>%
    filter(GEBIET_NAME == municipality) %>%
    pull(BFS_NR) %>%
    first()

  merger_info <- mergers %>% filter(new_BFS_Nr == new_bfs)

  if (nrow(merger_info) == 0) {
    message("No merger found for '", municipality, "'. Use plot_income() instead.")
    return(invisible(NULL))
  }

  merge_year <- merger_info$merge_year[1]
  old_bfs_nrs <- merger_info$old_BFS_Nr

  # Old municipalities: data up to (not including) merge year
  old_data <- kt_raw %>%
    filter(BFS_NR %in% old_bfs_nrs, INDIKATOR_JAHR < merge_year) %>%
    select(INDIKATOR_JAHR, INCOME_VALUE, label = GEBIET_NAME) %>%
    mutate(line_type = "pre-merger")

  # New municipality: data from merge year onwards
  new_data <- kt_data %>%
    filter(GEBIET_NAME == municipality, INDIKATOR_JAHR >= merge_year) %>%
    select(INDIKATOR_JAHR, INCOME_VALUE, label = GEBIET_NAME) %>%
    mutate(line_type = "post-merger")

  # Connector: dotted line from each old municipality's last point to new municipality's first point
  last_old <- old_data %>%
    group_by(label) %>%
    slice_max(INDIKATOR_JAHR) %>%
    ungroup()

  new_at_merge <- new_data %>%
    filter(INDIKATOR_JAHR == min(INDIKATOR_JAHR)) %>%
    select(INDIKATOR_JAHR, INCOME_VALUE)

  connector_data <- bind_rows(
    last_old %>% select(label, INDIKATOR_JAHR, INCOME_VALUE) %>% mutate(line_type = "pre-merger"),
    last_old %>% select(label) %>%
      mutate(INDIKATOR_JAHR = new_at_merge$INDIKATOR_JAHR,
             INCOME_VALUE   = new_at_merge$INCOME_VALUE,
             line_type      = "pre-merger")
  )

  # Order labels: merged municipality first, then old municipalities
  level_order <- c(municipality, unique(old_data$label))
  lw_overrides <- ifelse(level_order == municipality, 1.5, 0.8)

  plot_data <- bind_rows(old_data, new_data) %>%
    mutate(label = factor(label, levels = level_order))
  connector_data <- connector_data %>%
    mutate(label = factor(label, levels = level_order))
  all_years <- sort(unique(plot_data$INDIKATOR_JAHR))

  ggplot(plot_data, aes(x = INDIKATOR_JAHR, y = INCOME_VALUE,
                        colour = label, linewidth = line_type)) +
    geom_line() +
    geom_point(data = plot_data, size = 2) +
    geom_line(data = connector_data, linetype = "dotted") +
    geom_vline(xintercept = merge_year, linetype = "dashed", colour = "grey50") +
    annotate("text", x = merge_year, y = Inf,
             label = paste("Merger", merge_year),
             hjust = -0.1, vjust = 1.5, colour = "grey40", size = 3) +
    scale_linewidth_manual(values = c("pre-merger" = 0.8, "post-merger" = 1.5)) +
    scale_x_continuous(breaks = all_years) +
    scale_y_continuous(labels = scales::comma) +
    guides(
      colour = guide_legend(override.aes = list(linewidth = lw_overrides)),
      linewidth = "none"
    ) +
    labs(
      title = paste("Median Income:", municipality, "(incl. pre-merger municipalities)"),
      x = "",
      y = "Median Income (CHF)",
      colour = "Municipality"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_income_merged(income_kt, income_kt_raw, merger_mapping, "Stammheim")

# Line plot showing population (Einwohner) over time for a selected municipality.
# For Zürich city districts, pass the district name via `stadtkreis`.
plot_population <- function(data, municipality, stadtkreis = NULL) {

  df <- data %>%
    filter(Gemeindename == municipality)

  if (!is.null(stadtkreis)) {
    df <- df %>% filter(Stadtkreis_Name == stadtkreis)
  }

  df <- df %>%
    distinct(Ausgangsjahr, .keep_all = TRUE)

  title_label <- if (!is.null(stadtkreis)) {
    paste0(municipality, " - ", stadtkreis)
  } else {
    municipality
  }

  ggplot(df, aes(x = Ausgangsjahr, y = Einwohner)) +
    geom_line(colour = "#4DAC26", linewidth = 1.5) +
    geom_point(colour = "#4DAC26", size = 2) +
    scale_x_continuous(breaks = unique(df$Ausgangsjahr)) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = paste("Population:", title_label),
      x = "",
      y = "Population (Einwohner)"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_population(ebd, "Adliswil")
plot_population(ebd, "Zürich", stadtkreis = "Kreis 4")

# Line plot showing burglary rate (Einbrüche insgesamt per 1,000 inhabitants)
# over time for a selected municipality.
# For Zürich city districts, pass the district name via `stadtkreis`.

plot_ebd_rate <- function(data, municipality, stadtkreis = NULL) {

  df <- data %>%
    filter(Gemeindename == municipality,
           Tatbestand == "Einbrüche insgesamt")

  if (!is.null(stadtkreis)) {
    df <- df %>% filter(Stadtkreis_Name == stadtkreis)
  }

  df <- df %>%
    mutate(ebd_rate = (Straftaten_total / Einwohner) * 1000)

  title_label <- if (!is.null(stadtkreis)) {
    paste0(municipality, " - ", stadtkreis)
  } else {
    municipality
  }

  ggplot(df, aes(x = Ausgangsjahr, y = ebd_rate)) +
    geom_line(colour = "#D6604D", linewidth = 1.5) +
    geom_point(colour = "#D6604D", size = 2) +
    scale_x_continuous(breaks = unique(df$Ausgangsjahr)) +
    labs(
      title = paste("Burglary Rate:", title_label),
      x = "",
      y = "Burglaries per 1,000 Inhabitants"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_ebd_rate(ebd, "Adliswil")
plot_ebd_rate(ebd, "Zürich", stadtkreis = "Kreis 4")
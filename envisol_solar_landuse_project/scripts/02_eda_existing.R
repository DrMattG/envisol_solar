# ============================================================
# 02_eda_existing.R
# Exploratory analysis of existing and planned solar facilities
# ============================================================

# ---- Packages ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(summarytools)
})

Sys.setlocale(locale = "no_NB.utf8")

# ---- Paths ---------------------------------------------------------------

dir_processed <- "data/processed"
dir_figures   <- "outputs/figures"
dir_tables    <- "outputs/tables"
# create the folders if not already there
dirs <- c("data/processed", "outputs/figures", "outputs/tables")

invisible(lapply(dirs, function(d) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}))

# ---- Read processed data -------------------------------------------------

solar_energy <- read_csv(
  file.path(dir_processed, "solar_facilities_energy_density.csv"),
  show_col_types = FALSE
)

landcover_perc <- read_csv(
  file.path(dir_processed, "landcover_facility_percent.csv"),
  show_col_types = FALSE
)

landcover_mean <- read_csv(
  file.path(dir_processed, "landcover_mean_proportions.csv"),
  show_col_types = FALSE
)

landcover_perc
landcover_mean


# ---- Basic structure checks ---------------------------------------------

solar_energy <- solar_energy %>%
  mutate(
    agri_pv = factor(agri_pv, levels = c(0, 1),
                     labels = c("Conventional", "Agri-PV"))
  )


# Filtering ---------------------------------------------------------------

freq(solar_energy$stadium_katego)

# ---- Summary statistics: project info  ---------------------------------

# 1 = Under behandling
# 2 = Konsesjon gitt
# 3 = Konsesjon avslått
# 4 = Planlegging avsluttet
# 5 = Utbygd
# 6 = Under bygging

summarytools::descr(solar_energy)
summarytools::dfSummary(solar_energy)
view(dfSummary(solar_energy))

## 0) Quick numeric check: show the top values
sort(solar_energy$mwp_km2, decreasing = TRUE, na.last = NA)[1:10]

## 1) Histogram + rug (shows single extreme value clearly)
hist(solar_energy$mwp_km2, breaks = 20,
     main = "MWp density (MWp per km^2)",
     xlab = "MWp per km^2")
rug(solar_energy$mwp_km2)

## 2) Boxplot (outliers shown as points)
boxplot(solar_energy$mwp_km2, horizontal = TRUE,
        main = "Boxplot: MWp density",
        xlab = "MWp per km^2")

## 3) Dotchart sorted (best for spotting the single offender + label it)
o <- order(solar_energy$mwp_km2, decreasing = TRUE, na.last = NA)
dotchart(solar_energy$mwp_km2[o],
         labels = solar_energy$anleggnavn[o],
         cex = 0.6,
         main = "MWp density by project (sorted)",
         xlab = "MWp per km^2")

## 4) Simple scatter diagnostics: density vs area and vs MWp
plot(solar_energy$area_km2, solar_energy$mwp_km2,
     xlab = "Area (km^2)", ylab = "MWp per km^2",
     main = "Density vs area")
text(solar_energy$area_km2, solar_energy$mwp_km2,
     labels = ifelse(solar_energy$mwp_km2 > 150, solar_energy$anleggnavn, ""),
     pos = 4, cex = 0.7)

plot(solar_energy$mwp, solar_energy$mwp_km2,
     xlab = "MWp", ylab = "MWp per km^2",
     main = "Density vs MWp")
text(solar_energy$mwp, solar_energy$mwp_km2,
     labels = ifelse(solar_energy$mwp_km2 > 150, solar_energy$anleggnavn, ""),
     pos = 4, cex = 0.7)

## 5) Identify the worst case + print key fields (no guessing)
i <- which.max(solar_energy$mwp_km2)
solar_energy[i, c("anleggnavn","kommune","area_m2","area_km2","mwp","effekt_mw","prod_gwh","mwp_km2","mw_km2","gwh_km2")]


# ---- Summary statistics: energy density ---------------------------------

# Packages
library(dplyr)
library(tidyr)
library(writexl)

# ---- 1) Numeric summary (one cell per stat) ----
num_vars <- c("area_km2", "mwp", "effekt_mw", "prod_gwh",
              "mw_km2", "mwp_km2", "gwh_km2")

num_summary_wide <- solar_energy %>%
  summarise(across(all_of(num_vars),
                   list(
                     n       = ~sum(!is.na(.)),
                     missing = ~sum(is.na(.)),
                     mean    = ~mean(., na.rm = TRUE),
                     sd      = ~sd(., na.rm = TRUE),
                     median  = ~median(., na.rm = TRUE),
                     iqr     = ~IQR(., na.rm = TRUE),
                     min     = ~min(., na.rm = TRUE),
                     max     = ~max(., na.rm = TRUE)
                   ),
                   .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("variable", "stat"),
               names_sep = "__",
               values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  relocate(variable, n, missing, mean, sd, median, iqr, min, max)

# ---- 2) Frequency tables for categorical / binary vars ----
# Add/remove variables as you like
cat_vars <- c("stadium_katego", "stadium", "fylkenavn",
              "agri_pv", "beiting_planlagt", "grått_areal", "sambruk")

freq_table <- bind_rows(lapply(cat_vars, function(v) {
  x <- solar_energy[[v]]
  # Treat 0/1 numeric as factor for clearer output
  if (is.numeric(x) && all(na.omit(x) %in% c(0, 1))) x <- factor(x, levels = c(0, 1))
  tibble(value = x) %>%
    filter(!is.na(value)) %>%
    count(value, name = "n") %>%
    mutate(
      variable = v,
      percent = 100 * n / sum(n)
    ) %>%
    relocate(variable)
})) %>%
  arrange(variable, desc(n))

# ---- 3) Your custom “energy_summary” row + fivenum ----
energy_summary <- solar_energy %>%
  summarise(
    n_facilities  = n(),
    n_density     = sum(!is.na(mw_km2)),
    mean_mw_km2   = mean(mw_km2, na.rm = TRUE),
    sd_mw_km2     = sd(mw_km2, na.rm = TRUE),
    median_mw_km2 = median(mw_km2, na.rm = TRUE),
    iqr_mw_km2    = IQR(mw_km2, na.rm = TRUE)
    # If you actually have this column, uncomment:
    # , mean_mw_km2_alt = mean(mw_km2_alternative, na.rm = TRUE)
  )

five_mwp_km2 <- fivenum(solar_energy$mwp_km2)
fivenum_sheet <- data.frame(
  stat  = c("min", "Q1", "median", "Q3", "max"),
  value = as.numeric(five_mwp_km2)
)

# ---- 4) Write Excel workbook (multi-sheet) ----
out_file <- file.path(dir_tables, "solar_energy_summary.xlsx")

library(writexl)

write_xlsx(
  list(
    numeric_summary = num_summary_wide,
    freq_summary    = freq_table,
    energy_summary  = energy_summary,
    fivenum_mwp_km2 = fivenum_sheet
  ),
  path = out_file
)

out_file


# ---- Summary by facility type -------------------------------------------

energy_by_type <- solar_energy %>%
  group_by(agri_pv) %>%
  summarise(
    n                = n(),
    mean_mw_km2      = mean(mw_km2, na.rm = TRUE),
    median_mw_km2    = median(mw_km2, na.rm = TRUE),
    sd_mw_km2        = sd(mw_km2, na.rm = TRUE),
    .groups = "drop"
  )

energy_by_type

write_csv(
  energy_by_type,
  file.path(dir_tables, "energy_density_by_type.csv")
)

# ---- Distribution plots -------------------------------------------------

p_density_hist <- ggplot(solar_energy, aes(x = mw_km2)) +
  geom_histogram(bins = 20, colour = "white", fill = "grey70") +
  labs(
    x = "Energy density (MWp per km²)",
    y = "Number of facilities"
  ) +
  theme_classic()

p_density_hist

ggsave(
  file.path(dir_figures, "energy_density_histogram.png"),
  p_density_hist,
  width = 16, height = 12, units = "cm", dpi = 300
)

# ---- Boxplot: Agri-PV vs conventional -----------------------------------

p_density_box <- ggplot(solar_energy, aes(x = agri_pv, y = mw_km2, fill = agri_pv)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, alpha = 0.6) +
  geom_jitter(aes(color = agri_pv), width = 0.15, alpha = 0.5, size = 2) +
  labs(
    x = NULL,
    y = "Energy density (MWp per km²)",
    fill = "Category",
    color = "Category"
  ) +
  theme_classic() +
  theme(legend.position = "right")

p_density_box

ggsave(
  file.path(dir_figures, "energy_density_boxplot_agri_vs_conventional.png"),
  p_density_box,
  width = 16, height = 12, units = "cm", dpi = 300
)

# ---- Land cover composition (mean proportions) --------------------------

landcover_mean_plot <- landcover_mean %>%
  arrange(desc(proportion)) %>%
  ggplot(aes(x = reorder(landcover, proportion), y = proportion)) +
  geom_col(fill = "grey60") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Mean proportion of facility area"
  ) +
  theme_classic()

landcover_mean_plot

ggsave(
  file.path(dir_figures, "mean_landcover_proportions.png"),
  landcover_mean_plot,
  width = 18, height = 14, units = "cm", dpi = 300
)

# ---- Facility-level land cover variability ------------------------------

landcover_long <- landcover_perc %>%
  pivot_longer(
    cols = -anleggnavn,
    names_to = "landcover",
    values_to = "percent"
  )

landcover_variability <- landcover_long %>%
  group_by(landcover) %>%
  summarise(
    mean_percent = mean(percent, na.rm = TRUE),
    sd_percent   = sd(percent, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  landcover_variability,
  file.path(dir_tables, "landcover_variability_summary.csv")
)

landcover_variability

# ---- End of script ------------------------------------------------------


# landcover_perc: columns = anleggnavn + land cover classes (in %)
# solar_energy: contains anleggnavn + area_km2

# 1) Make long table and compute km2 in each class
lc_cols <- setdiff(names(landcover_perc), "anleggnavn")

lc_long <- merge(
  landcover_perc,
  solar_energy[, c("anleggnavn", "area_km2")],
  by = "anleggnavn",
  all.x = TRUE
)

lc_long <- reshape(
  lc_long,
  varying = lc_cols,
  v.names = "percent",
  timevar = "landcover",
  times = lc_cols,
  direction = "long"
)

lc_long$km2_in_class <- lc_long$area_km2 * (lc_long$percent / 100)

# Total project footprint (km2), counted once per project
total_area_km2 <- sum(solar_energy$area_km2, na.rm = TRUE)

# 2) Mean/SD of percent (project-average composition)
stats <- aggregate(percent ~ landcover, data = lc_long,
                   FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                       sd   = sd(x, na.rm = TRUE)))

stats_out <- data.frame(
  landcover     = stats$landcover,
  mean_percent  = stats$percent[, "mean"],
  sd_percent    = stats$percent[, "sd"]
)

# 3) Total km2 per class (area-weighted absolute overlap)
totals <- aggregate(km2_in_class ~ landcover, data = lc_long, sum, na.rm = TRUE)
names(totals)[2] <- "total_km2"

# 4) Merge + weighted percent of total footprint
table_landcover <- merge(stats_out, totals, by = "landcover", all = TRUE)
table_landcover$weighted_percent <- 100 * table_landcover$total_km2 / total_area_km2

# 5) Nice ordering (largest total overlap first)
table_landcover <- table_landcover[order(-table_landcover$total_km2), ]
table_landcover

library(writexl)
write_xlsx(list(landcover_summary = table_landcover),
           path = file.path(dir_tables, "landcover_summary.xlsx"))

# ---- Export inputs for Monte Carlo (capacity density sampling) -----------

# Goal: create a clean, reusable density “sample frame” that MC can bootstrap from.
# Put this in data/processed so it behaves like a derived input, not an output table.

density_sample <- solar_energy %>%
  transmute(
    anleggnavn,
    agri_pv,
    stadium_katego,
    stadium,
    density_mwp_km2 = mwp_km2,
    density_mw_ac_km2 = mw_km2,
    source_date = as.Date("2026-01-01")
  ) %>%
  # keep only valid density values (available for 75 projects in your data)
  filter(!is.na(density_mwp_km2), is.finite(density_mwp_km2)) %>%
  # optional: keep an explicit “unknown” level instead of NA (helps later grouping)
  mutate(
    agri_pv = forcats::fct_explicit_na(agri_pv, na_level = "Unknown")
  ) %>%
  arrange(anleggnavn)

# Quick sanity checks
stopifnot(nrow(density_sample) > 0)
stopifnot(all(density_sample$density_mwp_km2 > 0))

# Write a stable processed input for the Monte Carlo script
write_csv(
  density_sample,
  file.path(dir_processed, "project_density_sample_2026-01.csv")
)

# Optional convenience: also write a simple one-column vector file
# (useful if you want the absolute minimal read in the MC script)
write_csv(
  density_sample %>% select(density_mwp_km2),
  file.path(dir_processed, "project_density_mwp_km2_vector_2026-01.csv")
)


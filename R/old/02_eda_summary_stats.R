## DEPRECIATED

# # ---- Packages ----
# library(dplyr)
# library(tidyr)
# library(janitor)
# library(purrr)
# 
# Sys.setlocale(locale = "no_NB.utf8")
# 
# # ---- Helper: numeric summary function ----
# num_summary <- function(x) {
#   x <- as.numeric(x)
#   tibble(
#     n       = sum(!is.na(x)),
#     min     = suppressWarnings(min(x, na.rm = TRUE)),
#     q05     = suppressWarnings(quantile(x, 0.05, na.rm = TRUE, names = FALSE)),
#     q25     = suppressWarnings(quantile(x, 0.25, na.rm = TRUE, names = FALSE)),
#     median  = suppressWarnings(median(x, na.rm = TRUE)),
#     mean    = suppressWarnings(mean(x, na.rm = TRUE)),
#     q75     = suppressWarnings(quantile(x, 0.75, na.rm = TRUE, names = FALSE)),
#     q95     = suppressWarnings(quantile(x, 0.95, na.rm = TRUE, names = FALSE)),
#     max     = suppressWarnings(max(x, na.rm = TRUE)),
#     sd      = suppressWarnings(sd(x, na.rm = TRUE))
#   )
# }
# 
# # ---- 1) Counts for categorical variables ----
# stadium_counts <- solar_energy %>%
#   count(stadium, sort = TRUE) %>%
#   mutate(pct = n / sum(n))
# 
# saksKatego_counts <- solar_energy %>%
#   count(saksKatego, sort = TRUE) %>%
#   mutate(pct = n / sum(n))
# 
# agripv_counts <- solar_energy %>%
#   count(agri_pv, sort = TRUE) %>%
#   mutate(pct = n / sum(n))
# 
# sambruk_counts <- solar_energy %>%
#   count(Sambruk, sort = TRUE) %>%
#   mutate(pct = n / sum(n))
# 
# # ---- 2) "Count/number" for indicator-style variables ----
# # Assumes 1 = yes, 0 = no (as in your glimpse). Adjust if needed.
# indicator_counts <- solar_energy %>%
#   summarise(
#     n_total = n(),
#     n_nydyrking_planlagt_1 = sum(nydyrking_planlagt == 1, na.rm = TRUE),
#     n_beiting_planlagt_1   = sum(beiting_planlagt == 1, na.rm = TRUE),
#     n_gratt_areal_1        = sum(`Grått areal` == 1, na.rm = TRUE)
#   ) %>%
#   pivot_longer(-n_total, names_to = "metric", values_to = "n") %>%
#   mutate(pct = n / n_total)
# 
# # ---- 3) Numeric summaries (min/max/median/mean/sd/quantiles) ----
# numeric_vars <- c(
#   "areal_oppgitt_daa",
#   "area_m2",
#   "area_km2",
#   "MWp",
#   "GWh",
#   "MW/km2"   # non-syntactic name, handled via .data pronoun below
# )
# 
# numeric_summary_table <- tibble(variable = numeric_vars) %>%
#   mutate(stats = map(variable, ~ num_summary(solar_energy[[.x]]))) %>%
#   unnest(stats)
# 
# # ---- 4) Optional: summaries by stadium (often useful) ----
# numeric_by_stadium <- solar_energy %>%
#   group_by(stadium) %>%
#   summarise(across(all_of(numeric_vars), ~ list(num_summary(.x)), .names = "{.col}"), .groups = "drop") %>%
#   pivot_longer(-stadium, names_to = "variable", values_to = "stats") %>%
#   unnest(stats)
# 
# # ---- Print results ----
# stadium_counts
# saksKatego_counts
# agripv_counts
# sambruk_counts
# indicator_counts
# numeric_summary_table
# 
# # If you want the by-stadium numeric table:
# numeric_by_stadium
# 
# # ---- Optional: write outputs to CSV ----
# # write.csv(stadium_counts, "stadium_counts.csv", row.names = FALSE)
# # write.csv(saksKatego_counts, "saksKatego_counts.csv", row.names = FALSE)
# # write.csv(agripv_counts, "agripv_counts.csv", row.names = FALSE)
# # write.csv(sambruk_counts, "sambruk_counts.csv", row.names = FALSE)
# # write.csv(indicator_counts, "indicator_counts.csv", row.names = FALSE)
# # write.csv(numeric_summary_table, "numeric_summary_table.csv", row.names = FALSE)
# # write.csv(numeric_by_stadium, "numeric_by_stadium.csv", row.names = FALSE)
# 

# ============================================================
# 04_scenarios_montecarlo_v2.R
# Monte Carlo scenario analysis: capacity -> land area -> land cover
# Two spatial allocations:
#   (a) BAU: proportional to observed land-cover composition
#   (c) Agri+Grey: fixed agri/grey split (sensitivity 30/50/70), Agri factor = 1.5
# Uncertainty: capacity density + land-cover proportions (Dirichlet)
# No uncertainty on scenario MWp (assumed trajectory)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(scales)
})

set.seed(123)  # reproducibility

Sys.setlocale(locale = 'no_NB.utf8')
options(scipen = 999)

# ---- Paths ---------------------------------------------------------------

dir_raw       <- "data/raw"
dir_processed <- "data/processed"
dir_figures   <- "outputs/figures"
dir_tables    <- "outputs/tables"

dirs <- c(dir_raw, dir_processed, dir_figures, dir_tables)
invisible(lapply(dirs, function(d) dir.create(d, recursive = TRUE, showWarnings = FALSE)))

# ---- Read empirical capacity density sample for bootstrapping -----------

density_sample <- read_csv(
  file.path(dir_processed, "project_density_sample_2026-01.csv"),
  show_col_types = FALSE
)

density_pool <- density_sample$density_mwp_km2
density_pool <- density_pool[is.finite(density_pool) & !is.na(density_pool)]

stopifnot(length(density_pool) > 0)

head(density_sample)
glimpse(density_sample)

# ---- Monte Carlo settings ------------------------------------------------

n_iter <- 5000

# ---- Helper functions ----------------------------------------------------

# rtruncnorm <- function(n, mean, sd, lower = -Inf, upper = Inf) {
#   out <- rnorm(n, mean, sd)
#   while (any(out < lower | out > upper)) {
#     idx <- which(out < lower | out > upper)
#     out[idx] <- rnorm(length(idx), mean, sd)
#   }
#   out
# }

rdirichlet1 <- function(alpha) {
  x <- rgamma(length(alpha), shape = alpha, rate = 1)
  x / sum(x)
}

q_summ <- function(x) {
  tibble(
    mean   = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    p05    = unname(quantile(x, 0.05, na.rm = TRUE)),
    p95    = unname(quantile(x, 0.95, na.rm = TRUE))
  )
}

# ===================================================================
# Fixed scenario decisions
# ===================================================================

# # Capacity density MWp/km2: empirical planned-project distribution
# density_mean <- 79.4
# density_sd   <- 23.4
# density_lower <- 10     # keep positive + avoid pathological draws
# density_upper <- 250
# 
# draw_capacity_density <- function(n) {
#   rtruncnorm(n, mean = density_mean, sd = density_sd,
#              lower = density_lower, upper = density_upper)
# }
# 
# Capacity density MWp/km2: bootstrap from empirical project sample
# 
# ===================================================================
# Capacity density uncertainty (choose ONE option)
# ===================================================================

# ---- Option A (recommended): bootstrap empirical densities ----------
# Uses density_pool read from data/processed/project_density_sample_2026-01.csv
draw_capacity_density <- function(n) {
  sample(density_pool, size = n, replace = TRUE)
}

# ---- Option B (fallback): quantile-calibrated lognormal -------------
# Comment OUT Option A above and comment IN this block when you lack raw data.
#
# Provide robust summary stats (median, IQR, min, max) from your best sources.
# Here shown with your Norway MWp/km2 numbers as an example:

# density_median <- 77.5
# density_iqr    <- 35.6
# density_min    <- 36.3
# density_max    <- 135.6
#
# # Convert (median, IQR) -> lognormal parameters via Q1/Q3
# # For a lognormal: median = exp(mu), and Q3/Q1 = exp(2*z* sigma) where z = qnorm(0.75)
# q_to_lognorm_params <- function(median, iqr) {
#   q1 <- median - iqr / 2
#   q3 <- median + iqr / 2
#   stopifnot(q1 > 0, q3 > 0)
#   z <- qnorm(0.75)
#   sigma <- log(q3 / q1) / (2 * z)
#   mu <- log(median)
#   list(mu = mu, sigma = sigma)
# }
#
# rtrunc_lognorm <- function(n, mu, sigma, lower = 0, upper = Inf) {
#   out <- rlnorm(n, meanlog = mu, sdlog = sigma)
#   while (any(out < lower | out > upper)) {
#     idx <- which(out < lower | out > upper)
#     out[idx] <- rlnorm(length(idx), meanlog = mu, sdlog = sigma)
#   }
#   out
# }
#
# params <- q_to_lognorm_params(density_median, density_iqr)
#
# draw_capacity_density <- function(n) {
#   rtrunc_lognorm(
#     n,
#     mu = params$mu,
#     sigma = params$sigma,
#     lower = density_min,
#     upper = density_max
#   )
# }



# Agri-PV co-use area factor (fixed)
agri_area_factor <- 1.5

# Agri/Grey split sensitivity set (agri share; grey share = 1 - agri)
agri_shares <- c(0.30, 0.50, 0.70)

# Land-cover grouping for Agri+Grey allocation
agri_lc <- c("Cropland", "Grassland")
grey_lc <- c("Settlements and other artificial areas")

# Dirichlet concentration for land-cover composition uncertainty
dirichlet_concentration <- 500  # try 100 (more uncertainty) to 2000 (less)

# ---- Read scenario inputs ------------------------------------------------

scenarios_raw <- read_delim(
  file.path(dir_raw, "Scenario_calculator.csv"),
  delim = ";",
  show_col_types = FALSE
)

scenarios <- scenarios_raw |>
  rename(
    year       = Year,
    low_mwp    = `1 cumulative capacity (MWp)`,
    medium_mwp = `2 cumulative capacity (MWp)`,
    high_mwp   = `3 cumulative capacity (MWp)`
  ) |>
  mutate(year = readr::parse_number(year)) |>
  select(year, low_mwp, medium_mwp, high_mwp) |>
  filter(year <= 2050) |>
  arrange(year) |>
  # IMPORTANT: remove the first-year duplicate in-script (keep the larger/most complete value)
  group_by(year) |>
  summarise(
    low_mwp    = max(low_mwp, na.rm = TRUE),
    medium_mwp = max(medium_mwp, na.rm = TRUE),
    high_mwp   = max(high_mwp, na.rm = TRUE),
    .groups = "drop"
  )

scenarios_long <- scenarios |>
  pivot_longer(cols = -year, names_to = "scenario", values_to = "mwp") |>
  mutate(
    scenario = case_when(
      scenario == "low_mwp"    ~ "Low growth",
      scenario == "medium_mwp" ~ "Medium growth",
      scenario == "high_mwp"   ~ "High growth"
    ),
    scenario = factor(scenario, levels = c("Low growth", "Medium growth", "High growth"))
  )

median(density_sample$density_mwp_km2)

scenarios_raw

print(scenarios, n = 50)
glimpse(scenarios)
scenarios_long

plot(scenarios$high_mwp ~ scenarios$year)
plot(scenarios$medium_mwp ~ scenarios$year, col = "blue")
plot(scenarios$low_mwp ~ scenarios$year)


# ---- Read empirical land-cover proportions -------------------------------

landcover_mean <- read_csv(
  file.path(dir_processed, "landcover_mean_proportions.csv"),
  show_col_types = FALSE
) |>
  mutate(proportion = as.numeric(proportion))

stopifnot(abs(sum(landcover_mean$proportion) - 1) < 1e-6)

lc_mean <- landcover_mean |> arrange(landcover)
lc_names <- lc_mean$landcover
lc_p     <- lc_mean$proportion
alpha    <- lc_p * dirichlet_concentration

# ---- Colours -------------------------------------------------------------

land_cover_colors <- c(
  "Cropland" = "#FFD16E",
  "Grassland" = "#FFFFAD",
  "Forest and woodlands" = "#9ECC73",
  "Heathland and shrub" = "#D9D9D9",
  "Inland wetlands" = "#CBCBFB",
  "Settlements and other artificial areas" = "#FCD9D6",
  "Lakes and reservoirs" = "#91E7FF",
  "Rivers and canals" = "#91E7FF",
  "Sparsely vegetated ecosystems" = "#E3E3E3"
)

# ---- Land-cover groups for summary tables/figures ------------------------

to_group <- function(lc) {
  case_when(
    lc == "Forest and woodlands" ~ "Forest & woodlands",
    lc %in% agri_lc              ~ "Agriculture",
    lc %in% grey_lc              ~ "Grey/modified",
    lc %in% c("Lakes and reservoirs", "Rivers and canals") ~ "Water",
    TRUE                         ~ "Other semi-natural"
  )
}

landcover_mean
lc_mean
land_cover_colors
to_group()

# ============================================================
# Monte Carlo simulation
# ============================================================

sim_one_iter <- function(iter_id) {
  
  # Draw uncertain parameters
  density_mwp_km2 <- draw_capacity_density(1)
  
  # Draw land-cover proportions (Dirichlet) around observed mean
  lc_draw <- rdirichlet1(alpha)
  lc_tbl <- tibble(
    landcover  = lc_names,
    proportion = lc_draw
  )
  
  # Scenario MWp (no uncertainty)
  sc <- scenarios_long |>
    mutate(area_km2 = mwp / density_mwp_km2)
  
  # ---- (a) BAU allocation across all land-cover classes
  area_bau <- crossing(sc, lc_tbl) |>
    mutate(area_lc_km2 = area_km2 * proportion) |>
    mutate(iter = iter_id, allocation = "BAU", agri_share = NA_real_) |>
    select(iter, allocation, agri_share, year, scenario, landcover, area_km2, area_lc_km2)
  
  # ---- (c) Agri + Grey allocation (fixed split sensitivity)
  lc_grouped <- lc_tbl |>
    filter(landcover %in% c(agri_lc, grey_lc)) |>
    mutate(
      group = case_when(
        landcover %in% agri_lc ~ "Agri-PV",
        landcover %in% grey_lc ~ "Grey area"
      )
    ) |>
    group_by(group) |>
    mutate(prop_group = proportion / sum(proportion)) |>
    ungroup()
  
  # Expand scenarios by agri_share sensitivity values
  sc_ag <- crossing(sc, tibble(agri_share = agri_shares)) |>
    mutate(
      grey_share = 1 - agri_share,
      area_agri_km2 = agri_share * area_km2 * agri_area_factor,
      area_grey_km2 = grey_share * area_km2
    )
  
  area_agri_grey <- crossing(sc_ag, lc_grouped) |>
    mutate(
      area_lc_km2 = ifelse(
        group == "Agri-PV",
        area_agri_km2 * prop_group,
        area_grey_km2 * prop_group
      ),
      iter = iter_id,
      allocation = "Agri+Grey"
    ) |>
    select(iter, allocation, agri_share, year, scenario, landcover, area_km2, area_lc_km2)
  
  list(bau = area_bau, ag = area_agri_grey)
}

# Run MC (base loop)
sim_bau_list <- vector("list", n_iter)
sim_ag_list  <- vector("list", n_iter)

for (i in seq_len(n_iter)) {
  res <- sim_one_iter(i)
  sim_bau_list[[i]] <- res$bau
  sim_ag_list[[i]]  <- res$ag
  if (i %% 500 == 0) message("Completed ", i, " / ", n_iter)
}

sim_bau <- bind_rows(sim_bau_list)
sim_ag  <- bind_rows(sim_ag_list)

sim_all <- bind_rows(sim_bau, sim_ag)



# ============================================================
# Summaries (tables)
# ============================================================

# # Total area by year & scenario (BAU has agri_share = NA)
# total_area_summ <- sim_all |>
#   distinct(iter, allocation, agri_share, year, scenario, area_km2) |>
#   group_by(allocation, agri_share, year, scenario) |>
#   summarise(q_summ(area_km2), .groups = "drop")
# 
# write_csv(total_area_summ, file.path(dir_tables, "mc_total_area_km2_summary.csv"))
# 
# # Area by landcover
# area_lc_summ <- sim_all |>
#   group_by(allocation, agri_share, year, scenario, landcover) |>
#   summarise(q_summ(area_lc_km2), .groups = "drop")
# 
# write_csv(area_lc_summ, file.path(dir_tables, "mc_area_by_landcover_summary.csv"))
# 
# # Area by land-cover GROUP (recommended for main-text table/figure)
# area_group_summ <- sim_all |>
#   mutate(landcover_group = to_group(landcover)) |>
#   group_by(iter, allocation, agri_share, year, scenario, landcover_group) |>
#   summarise(area_group_km2 = sum(area_lc_km2), .groups = "drop") |>
#   group_by(allocation, agri_share, year, scenario, landcover_group) |>
#   summarise(q_summ(area_group_km2), .groups = "drop")
# 
# write_csv(area_group_summ, file.path(dir_tables, "mc_area_by_landcover_group_summary.csv"))
# 
# # Convenience tables for 2030 and 2050 (total + groups)
# years_key <- c(2030, 2050)
# 
# total_key <- total_area_summ |>
#   filter(year %in% years_key) |>
#   arrange(allocation, agri_share, scenario, year)
# 
# write_csv(total_key, file.path(dir_tables, "mc_total_area_key_years_2030_2050.csv"))
# 
# group_key_2050 <- area_group_summ |>
#   filter(year == 2050) |>
#   arrange(allocation, agri_share, scenario, landcover_group)
# 
# write_csv(group_key_2050, file.path(dir_tables, "mc_landcover_groups_2050.csv"))
# 
# # Grey area needed (Agri+Grey only)
# grey_needed <- sim_ag |>
#   filter(landcover %in% grey_lc) |>
#   group_by(iter, agri_share, year, scenario) |>
#   summarise(grey_km2 = sum(area_lc_km2), .groups = "drop") |>
#   group_by(agri_share, year, scenario) |>
#   summarise(q_summ(grey_km2), .groups = "drop") |>
#   mutate(allocation = "Agri+Grey")
# 
# write_csv(grey_needed, file.path(dir_tables, "mc_grey_area_needed_summary.csv"))
# 
# # Total area by year & scenario (BAU has agri_share = NA)
# total_area_summ <- sim_all |>
#   distinct(iter, allocation, agri_share, year, scenario, area_km2) |>
#   group_by(allocation, agri_share, year, scenario) |>
#   summarise(q_summ(area_km2), .groups = "drop")
#   
total_area_summ <- sim_all |>
  group_by(iter, allocation, agri_share, year, scenario) |>
  summarise(total_area_km2 = sum(area_lc_km2), .groups = "drop") |>
  group_by(allocation, agri_share, year, scenario) |>
  summarise(q_summ(total_area_km2), .groups = "drop")

head(total_area_summ)
glimpse(total_area_summ)

# sanity_check <- sim_all |>
#   group_by(iter, allocation, agri_share, year, scenario) |>
#   summarise(
#     base_area = first(area_km2),
#     total_area_km2 = sum(area_lc_km2),
#     .groups = "drop"
#   ) |>
#   filter(year %in% c(2030, 2050)) |>
#   head(20)
# 
# print(sanity_check, n = 30)

write_csv(total_area_summ, file.path(dir_tables, "mc_total_area_km2_summary.csv"))
writexl::write_xlsx(total_area_summ, file.path(dir_tables, "mc_total_area_km2_summary.xlsx"))

# Area by landcover
area_lc_summ <- sim_all |>
  group_by(allocation, agri_share, year, scenario, landcover) |>
  summarise(q_summ(area_lc_km2), .groups = "drop")

head(area_lc_summ)
glimpse(area_lc_summ)

write_csv(area_lc_summ, file.path(dir_tables, "mc_area_by_landcover_summary.csv"))
writexl::write_xlsx(area_lc_summ, file.path(dir_tables, "mc_area_by_landcover_summary.xlsx"))

# Area by land-cover GROUP (recommended for main-text table/figure)
area_group_summ <- sim_all |>
  mutate(landcover_group = to_group(landcover)) |>
  group_by(iter, allocation, agri_share, year, scenario, landcover_group) |>
  summarise(area_group_km2 = sum(area_lc_km2), .groups = "drop") |>
  group_by(allocation, agri_share, year, scenario, landcover_group) |>
  summarise(q_summ(area_group_km2), .groups = "drop")

head(area_group_summ)
glimpse(area_group_summ)

write_csv(area_group_summ, file.path(dir_tables, "mc_area_by_landcover_group_summary.csv"))
writexl::write_xlsx(area_group_summ, file.path(dir_tables, "mc_area_by_landcover_group_summary.xlsx"))

# Convenience tables for 2030 and 2050 (total + groups)
years_key <- c(2030, 2050)

total_key <- total_area_summ |>
  filter(year %in% years_key) |>
  arrange(allocation, agri_share, scenario, year)

print(total_key, n = 100)
glimpse(total_key)

write_csv(total_key, file.path(dir_tables, "mc_total_area_key_years_2030_2050.csv"))
writexl::write_xlsx(total_key, file.path(dir_tables, "mc_total_area_key_years_2030_2050.xlsx"))

group_key_2050 <- area_group_summ |>
  filter(year == 2050) |>
  arrange(allocation, agri_share, scenario, landcover_group)

group_key_2050
glimpse(group_key_2050)

write_csv(group_key_2050, file.path(dir_tables, "mc_landcover_groups_2050.csv"))
writexl::write_xlsx(group_key_2050, file.path(dir_tables, "mc_landcover_groups_2050.xlsx"))

# Grey area needed (Agri+Grey only)
grey_needed <- sim_ag |>
  filter(landcover %in% grey_lc) |>
  group_by(iter, agri_share, year, scenario) |>
  summarise(grey_km2 = sum(area_lc_km2), .groups = "drop") |>
  group_by(agri_share, year, scenario) |>
  summarise(q_summ(grey_km2), .groups = "drop") |>
  mutate(allocation = "Agri+Grey")

head(grey_needed)
glimpse(grey_needed)

write_csv(grey_needed, file.path(dir_tables, "mc_grey_area_needed_summary.csv"))
writexl::write_xlsx(grey_needed, file.path(dir_tables, "mc_grey_area_needed_summary.xlsx"))


# Agricultural area needed (Agri+Grey only)
agri_needed <- sim_ag |>
  filter(landcover %in% agri_lc) |>
  group_by(iter, agri_share, year, scenario) |>
  summarise(agri_km2 = sum(area_lc_km2), .groups = "drop") |>
  group_by(agri_share, year, scenario) |>
  summarise(q_summ(agri_km2), .groups = "drop") |>
  mutate(allocation = "Agri+Grey")

head(agri_needed)
glimpse(agri_needed)

write_csv(agri_needed, file.path(dir_tables, "mc_agricultural_area_needed_summary.csv"))
writexl::write_xlsx(agri_needed, file.path(dir_tables, "mc_agricultural_area_needed_summary.xlsx"))

# ---- Inspect outputs -----------------------------------------------------


# ============================================================
# Figures
# ============================================================

# ---- Figure 1 (main): 6 panels total area over time (Agri+Grey fixed at 50/50)

total_plot_df <- total_area_summ |>
  filter(is.na(agri_share) | agri_share == 0.50) |>
  mutate(
    allocation = factor(allocation, levels = c("BAU", "Agri+Grey")),
    scenario   = factor(scenario, levels = c("Low growth", "Medium growth", "High growth"))
  )

p_total <- ggplot(total_plot_df, aes(x = year, y = median, group = 1)) +
  geom_ribbon(aes(ymin = p05, ymax = p95), alpha = 0.25) +
  geom_line(linewidth = 1) +
  facet_grid(allocation ~ scenario, switch = "y") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Year",
    y = bquote("Total land area (" * km^2 * ")"),
    subtitle = paste0(
      "Land demand: Business as usual (BAU); AgriPV + Grey areas with 50/50 split."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.placement = "outside",
    strip.text.y = element_text(angle = 0),
    panel.grid.minor = element_blank()
  )

print(p_total)

ggsave(
  filename = file.path(dir_figures, "fig1_total_area_uncertainty_6panels.png"),
  plot = p_total,
  width = 30, height = 20, units = "cm", dpi = 300
)


# ---- Figure 1 (main): 6 panels total area over time (Agri+Grey fixed at 50/50)

total_plot_df <- total_area_summ |>
  filter(is.na(agri_share) | agri_share == 0.50) |>
  mutate(
    allocation = factor(allocation, levels = c("BAU", "Agri+Grey")),
    scenario   = factor(scenario, levels = c("Low growth", "Medium growth", "High growth"))
  )

label_df <- total_plot_df |>
  filter(year %in% c(2030, 2050)) |>
  mutate(
    label = format(round(median, 1), nsmall = 1)
  )

label_df <- total_plot_df |>
  filter(year %in% c(2030, 2050)) |>
  mutate(
    label = paste0(round(median, 1), "~km^2")
  )

p_total <- ggplot(total_plot_df, aes(x = year, y = median, group = 1)) +
  geom_ribbon(aes(ymin = p05, ymax = p95), alpha = 0.25) +
  geom_line(linewidth = 1) +
  geom_point(
    data = label_df,
    size = 2
  ) +
  geom_text(
    data = label_df,
    aes(label = label),
    parse = TRUE,
    vjust = -0.8,
    hjust = 0.8,
    size = 3.7,
    fontface = "bold"
  )+
  facet_grid(allocation ~ scenario, switch = "y") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.10))) +
  labs(
    x = "Year",
    y = bquote("Total land area (" * km^2 * ")"),
    subtitle = "Land demand: Business as usual (BAU); AgriPV + Grey areas with 50/50 split."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.placement = "outside",
    strip.text.y = element_text(angle = 0),
    panel.grid.minor = element_blank()
  )

print(p_total)

ggsave(
  filename = file.path(dir_figures, "fig1_total_area_uncertainty_6panels.png"),
  plot = p_total,
  width = 30, height = 20, units = "cm", dpi = 300
)

# ---- Figure 2 (main): 2050 land-cover GROUPS dot+interval (Agri+Grey 50/50; BAU)

group_plot_df <- area_group_summ |>
  filter(year == 2050) |>
  filter(is.na(agri_share) | agri_share == 0.50) |>
  mutate(
    allocation = factor(allocation, levels = c("BAU", "Agri+Grey")),
    scenario   = factor(scenario, levels = c("Low growth", "Medium growth", "High growth")),
    landcover_group = factor(
      landcover_group,
      levels = c("Forest & woodlands", "Agriculture", "Grey/modified", "Other semi-natural", "Water")
    )
  )

p_groups_2050 <- ggplot(group_plot_df, aes(x = landcover_group, y = median)) +
  geom_errorbar(aes(ymin = p05, ymax = p95), width = 0.15, linewidth = 0.6) +
  geom_point(size = 2) +
  facet_grid(allocation ~ scenario, switch = "y") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  coord_flip() +
  labs(
    x = NULL,
    y = bquote("Area in 2050 (" * km^2 * ")"),
    subtitle = "Land demand for solar energy in 2050: Business as Usual (BAU) vs Gray areas + Agri PV"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.placement = "outside",
    strip.text.y = element_text(angle = 0),
    panel.grid.minor = element_blank()
  )

print(p_groups_2050)

group_plot_df <- area_group_summ |>
  filter(year == 2050) |>
  filter(is.na(agri_share) | agri_share == 0.50) |>
  filter(landcover_group != "Water") |>
  mutate(
    allocation = factor(allocation, levels = c("BAU", "Agri+Grey")),
    scenario   = factor(scenario, levels = c("Low growth", "Medium growth", "High growth")),
    
    landcover_group = case_when(
      landcover_group == "Forest & woodlands" ~ "Forests and woodlands",
      TRUE ~ landcover_group
    ),
    
    landcover_group = factor(
      landcover_group,
      levels = c(
        "Grey/modified",
        "Other semi-natural",
        "Agriculture",
        "Forests and woodlands"
      )
    )
  )


landcover_group_colors <- c(
  "Forests and woodlands" = "#326633",
  "Agriculture" = "#F17925",
  "Other semi-natural" = "#FFAC00",
  "Grey/modified" = "#4B4C4E"
)


p_groups_2050 <- ggplot(group_plot_df,
                        aes(x = landcover_group, y = median, colour = landcover_group)) +
  geom_errorbar(aes(ymin = p05, ymax = p95), width = 0.15, linewidth = 0.6) +
  geom_point(size = 2.5) +
  geom_text(
    data = label_df,
    aes(label = label),
    parse = TRUE,
    vjust = -0.55,
    hjust = -0.15,
    size = 3.5,
    colour = "black"
  ) +
  facet_grid(allocation ~ scenario, switch = "y") +
  scale_colour_manual(values = landcover_group_colors) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.18))) +
  coord_flip() +
  labs(
    x = NULL,
    y = bquote("Area in 2050 (" * km^2 * ")"),
    subtitle = "Land demand for solar energy in 2050: Business as Usual (BAU) vs Grey areas + Agri-PV"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    strip.placement = "outside",
    strip.text.y = element_text(angle = 0),
    panel.grid.minor = element_blank()
  )


print(p_groups_2050)

ggsave(
  filename = file.path(dir_figures, "fig2_landcover_groups_2050_intervals_flipped.png"),
  plot = p_groups_2050,
  width = 30, height = 20, units = "cm", dpi = 300
)

# ---- Supplement: total area sensitivity to Agri share (Agri+Grey only)

p_total_sens <- total_area_summ |>
  filter(allocation == "Agri+Grey") |>
  mutate(
    agri_share = factor(
      agri_share,
      levels = agri_shares,
      labels = paste0("Agri share ", agri_shares * 100, "%")
    ),
    scenario = factor(scenario, levels = c("Low growth", "Medium growth", "High growth"))
  ) |>
  ggplot(aes(x = year, y = median, group = agri_share, colour = agri_share, fill = agri_share)) +
  geom_ribbon(aes(ymin = p05, ymax = p95), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1) +
  facet_wrap(~ scenario, nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Year",
    y = bquote("Total land area (" * km^2 * ")"),
    colour = NULL,
    fill = NULL,
    subtitle = paste0("Agri+Grey sensitivity to fixed Agri share (n = ", n_iter, ").")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p_total_sens)

ggsave(
  filename = file.path(dir_figures, "supp_total_area_agri_share_sensitivity.png"),
  plot = p_total_sens,
  width = 30, height = 12, units = "cm", dpi = 300
)

# --- Sanity check (new results) ---------------------------------------------

# --- Sanity check (new results) ---------------------------------------------

# Expect 3 fixed shares + NA (BAU)
total_area_summ |>
  count(agri_share)

# Grey-needed should only have the fixed shares (no NA)
grey_needed |>
  count(agri_share)

# Grey-needed must vary by agri_share (otherwise something is wrong)
grey_needed |>
  group_by(year, scenario) |>
  summarise(n_distinct_mean = n_distinct(mean), .groups = "drop") |>
  summarise(min_n = min(n_distinct_mean), max_n = max(n_distinct_mean))


# --- Plot: Grey area needed sensitivity (same styling as old plot) ----------

p_grey_needed_sens <- grey_needed |>
  mutate(
    agri_share = factor(
      agri_share,
      levels = agri_shares,
      labels = paste0("Agri share ", agri_shares * 100, "%")
    ),
    scenario = factor(scenario, levels = c("Low growth", "Medium growth", "High growth"))
  ) |>
  ggplot(aes(x = year, y = median, group = agri_share, colour = agri_share, fill = agri_share)) +
  geom_ribbon(aes(ymin = p05, ymax = p95), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1) +
  facet_wrap(~ scenario, nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Year",
    y = bquote("Grey area needed (" * km^2 * ")"),
    colour = NULL,
    fill = NULL,
    subtitle = paste0("Agri+Grey sensitivity to fixed Agri share (n = ", n_iter, ").")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Export (match your existing export pattern; adjust size if needed)
ggsave(
  filename = file.path(dir_figures, "mc_grey_area_needed_sensitivity.png"),
  plot = p_grey_needed_sens,
  width = 12, height = 5, dpi = 300
)


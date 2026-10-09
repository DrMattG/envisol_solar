# ============================================================
# 03_scenarios.R
# Scenario analysis: energy capacity -> land area -> ecosystems
# ============================================================

# ---- Packages ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
})

# ---- Paths ---------------------------------------------------------------

dir_raw       <- "data/raw"
dir_processed <- "data/processed"
dir_figures   <- "outputs/figures"
dir_tables    <- "outputs/tables"

# ---- Assumptions ---------------------------------------------------------
# Centralised here for transparency and later sensitivity analysis

capacity_density_km2 <- 74.5   # MWp per km² (baseline)
agri_share           <- 0.5    # Share of new capacity on agricultural land
grey_share           <- 0.5    # Share of new capacity on grey areas
agri_area_factor     <- 1.5    # Lower density for agri-PV (co-use)

# ---- Read scenario inputs ------------------------------------------------

scenarios_raw <- read_delim(
  file.path(dir_raw, "Scenario_calculator.csv"),
  delim = ";",
  show_col_types = FALSE
)

scenarios <- scenarios_raw %>%
  rename(
    year             = Year,
    low_mwp          = `1 cumulative capacity (MWp)`,
    medium_mwp       = `2 cumulative capacity (MWp)`,
    high_mwp         = `3 cumulative capacity (MWp)`
  ) %>%
  select(year, low_mwp, medium_mwp, high_mwp)

# ---- Read empirical land-cover proportions -------------------------------

landcover_mean <- read_csv(
  file.path(dir_processed, "landcover_mean_proportions.csv"),
  show_col_types = FALSE
)

# ---- Set land-cover colours -------------------------------

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


# ---- Reshape scenarios to long ------------------------------------------

scenarios_long <- scenarios %>%
  pivot_longer(
    cols = -year,
    names_to = "scenario",
    values_to = "mwp"
  ) %>%
  mutate(
    scenario = case_when(
      scenario == "low_mwp"    ~ "Low growth",
      scenario == "medium_mwp" ~ "Medium growth",
      scenario == "high_mwp"   ~ "High growth"
    ),
    scenario = factor(scenario,
                      levels = c("Low growth", "Medium growth", "High growth"))
  )

scenarios_long <- scenarios_long %>%
  filter(year <= 2050)

# ---- Convert capacity to land area --------------------------------------

scenarios_long <- scenarios_long %>%
  mutate(
    area_km2 = mwp / capacity_density_km2
  )

write_csv(
  scenarios_long,
  file.path(dir_tables, "scenario_area_total_km2.csv")
)

# ---- Business-as-usual land-cover allocation ----------------------------

area_bau <- crossing(scenarios_long, landcover_mean) %>%
  mutate(
    area_lc_km2 = area_km2 * proportion
  )

write_csv(
  area_bau,
  file.path(dir_tables, "scenario_area_by_landcover_bau.csv")
)

# ---- Plot: Business-as-usual --------------------------------------------

p_bau <- ggplot(
  area_bau,
  aes(x = factor(year), y = area_lc_km2, fill = landcover)
) +
  geom_col(color = "black") +
  facet_wrap(~scenario, ncol = 3) +
  labs(
    x = "Year",
    y = "Area (km²)",
    fill = "Ecosystem type",
    subtitle = "Business-as-usual: current land-cover proportions"
  ) +
  scale_fill_manual(values = land_cover_colors) +
  scale_x_discrete(breaks = seq(2025, 2050, 5)) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


ggsave(
  file.path(dir_figures, "scenario_bau_landcover.png"),
  p_bau,
  width = 28, height = 18, units = "cm", dpi = 300
)

# ---- Agri-PV + grey areas scenario --------------------------------------

# Define land-cover groups
agri_lc <- c("Cropland", "Grassland")
grey_lc <- c("Settlements and other artificial areas",
             "Sparsely vegetated ecosystems")

landcover_grouped <- landcover_mean %>%
  filter(landcover %in% c(agri_lc, grey_lc)) %>%
  mutate(
    group = case_when(
      landcover %in% agri_lc ~ "Agri-PV",
      landcover %in% grey_lc ~ "Grey area"
    )
  ) %>%
  group_by(group) %>%
  mutate(
    prop_group = proportion / sum(proportion)
  ) %>%
  ungroup()

area_agri_grey <- scenarios_long %>%
  mutate(
    area_agri_km2 = agri_share * area_km2 * agri_area_factor,
    area_grey_km2 = grey_share * area_km2
  )

area_split <- crossing(area_agri_grey, landcover_grouped) %>%
  mutate(
    area_lc_km2 = case_when(
      group == "Agri-PV"   ~ area_agri_km2 * prop_group,
      group == "Grey area" ~ area_grey_km2 * prop_group
    )
  )

write_csv(
  area_split,
  file.path(dir_tables, "scenario_area_by_landcover_agri_grey.csv")
)

# ---- Plot: Agri-PV + grey areas -----------------------------------------

p_agri_grey <- ggplot(
  area_split,
  aes(x = factor(year), y = area_lc_km2, fill = landcover)
) +
  geom_col(color = "black") +
  facet_wrap(~scenario, ncol = 3) +
  labs(
    x = "Year",
    y = "Area (km²)",
    fill = "Ecosystem type",
    subtitle = "50% Agri-PV (1.5× area), 50% grey areas"
  ) +
  scale_fill_manual(values = land_cover_colors) +
  scale_x_discrete(breaks = seq(2025, 2050, 5)) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


ggsave(
  file.path(dir_figures, "scenario_agri_grey_landcover.png"),
  p_agri_grey,
  width = 28, height = 18, units = "cm", dpi = 300
)

# ---- End of script ------------------------------------------------------

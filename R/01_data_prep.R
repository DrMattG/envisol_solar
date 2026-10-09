# ============================================================
# 01_data_prep.R
# Data preparation for solar land-use and energy density analysis
# ============================================================

# ---- Packages ------------------------------------------------------------

suppressPackageStartupMessages({
  library(NinaR)
  library(sf)
  library(dplyr)
  library(tidyr)
  library(readr)
})

# ---- Settings ------------------------------------------------------------

#getwd()

Sys.setlocale(locale = "no_NB.utf8")

# Project-relative paths
dir_raw       <- "data/raw"
dir_processed <- "data/processed"

# ---- Database connection -------------------------------------------------

postgreSQLConnect()

# ---- Read spatial data ---------------------------------------------------

solar_polygons <- st_read(
  file.path(dir_raw, "sol_jan_2026_final.gpkg"),
  quiet = TRUE
) %>%
  select(anleggnavn, everything())

glimpse(solar_polygons)

unique(solar_polygons$anleggnavn)

#Remove dubious outliers
# solar_polygons <- solar_polygons %>% filter(anleggnavn != "Løvbergsmoen solkraftverk")
# solar_polygons <- solar_polygons %>% filter(anleggnavn != "Vikersund solkraftverk")

solar_polygons[12, 7] <- 19

# ---- Read tabular energy data -------------------------------------------

# energy_density <- read_delim(
#   file.path(dir_raw, "solar_facilities_with_capacity_2025-04-28.csv"),
#   delim  = ";",
#   locale = locale(encoding = "UTF-8"),
#   show_col_types = FALSE
# )

# ---- Energy density metrics ---------------------------------------------

solar_polygons <- solar_polygons %>%
  mutate(
    # Ensure numeric area
    area_km2 = as.numeric(area_km2),
    
    # Parse MWp (stored as character)
    mwp = parse_number(mwp),
    
    # Energy/capacity density metrics (guard against missing/zero/negative area)
    mw_km2  = if_else(!is.na(effekt_mw) & area_km2 > 0, effekt_mw / area_km2, NA_real_),
    mwp_km2 = if_else(!is.na(mwp)   & area_km2 > 0, mwp   / area_km2, NA_real_),
    gwh_km2 = if_else(!is.na(prod_gwh)  & area_km2 > 0, prod_gwh  / area_km2, NA_real_),
  )

energy_density <- st_drop_geometry(solar_polygons)

energy_density <- energy_density %>%
 mutate(
   # mw_ha               = MWp / area_ha,
   # mw_km2              = MWp / area_km2,
   # mw_km2_alternative  = MWp / areal_oppgitt_daa * 1000,
   agri_pv             = factor(agri_pv)
 )

# ---- Save cleaned tabular data ------------------------------------------
#
write_csv(
 energy_density,
 file.path(dir_processed, "solar_facilities_energy_density.csv")
)

# ---- Clip solar polygons to land cover (PostGIS) ------------------------

solar_polygons_clipped <- clip_gis_layer(solar_polygons)

# ---- Land cover statistics function -------------------------------------

landcover_stats <- function(data, group_var) {
  data %>%
    st_drop_geometry() %>%
    group_by(anleggnavn, !!sym(group_var)) %>%
    summarise(
      area_m2 = sum(as.numeric(areal_m2), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(anleggnavn) %>%
    mutate(
      total_area_m2 = sum(area_m2, na.rm = TRUE),
      percent       = 100 * area_m2 / total_area_m2
    ) %>%
    ungroup()
}

# ---- Facility-level land cover stats ------------------------------------

lc_stats <- landcover_stats(
  solar_polygons_clipped,
  "okosystemtype_1"
)

# Area (m²), wide
lc_m2_wide <- lc_stats %>%
  select(anleggnavn, okosystemtype_1, area_m2) %>%
  pivot_wider(
    names_from  = okosystemtype_1,
    values_from = area_m2,
    values_fill = 0
  )

# Percent, wide
lc_perc_wide <- lc_stats %>%
  select(anleggnavn, okosystemtype_1, percent) %>%
  pivot_wider(
    names_from  = okosystemtype_1,
    values_from = percent,
    values_fill = 0
  )

# ---- Mean land cover proportions (for scenarios) ------------------------

landcover_mean_props <- lc_perc_wide %>%
  select(-anleggnavn) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(
    everything(),
    names_to  = "landcover",
    values_to = "percent"
  ) %>%
  mutate(
    proportion = percent / sum(percent)
  )

# ---- Save processed outputs ---------------------------------------------

write_csv(
  lc_m2_wide,
  file.path(dir_processed, "landcover_facility_area_m2.csv")
)

write_csv(
  lc_perc_wide,
  file.path(dir_processed, "landcover_facility_percent.csv")
)

write_csv(
  landcover_mean_props,
  file.path(dir_processed, "landcover_mean_proportions.csv")
)

st_write(
  solar_polygons,
  file.path(dir_processed, "solar_facilities_sf.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

# ---- End of script ------------------------------------------------------

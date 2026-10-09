# ENVISOL: solar energy and land use in Norway

R scripts and spatial data for analysing land use associated with existing and planned solar facilities in Norway, and exploring future land requirements under alternative solar-capacity and land-allocation scenarios.

The analysis links facility footprints, capacity and production information with land-cover data. It compares continuation of the observed land-cover mix with allocation to agricultural land and already modified (grey) areas. Separate scripts prepare facility polygons for use in NIBIO’s map-based greenhouse-gas calculator.

## What this repository does

- Calculates facility-level capacity density (MWp/km² and MW/km²) and annual production density (GWh/km²).
- Summarises land-cover composition within solar-facility footprints.
- Converts low-, medium- and high-growth capacity trajectories into land-area requirements.
- Compares business-as-usual (BAU) land allocation with an agricultural/grey-area allocation.
- Explores uncertainty in capacity density and land-cover composition through Monte Carlo simulation.
- Exports GeoJSON files for a separate greenhouse-gas assessment workflow.

Some scripts require NINA infrastructure and local data paths.

## Repository structure

| Location | Contents |
| --- | --- |
| `R/` | Data preparation, exploratory analysis, scenario modelling and spatial export scripts |
| `R/old/` | Earlier exploratory code |
| `data/raw/` | Facility GeoPackages, capacity information and scenario input tables |
| `data/processed/` | Derived capacity-density and land-cover tables, spatial data and GeoJSON exports |
| `outputs/figures/` | Analysis and scenario figures |
| `outputs/tables/` | Summary tables in CSV and Excel formats |
| Output subdirectories named `old` or `old_january_2026` | Earlier outputs retained for reference |

## Analysis scripts

| Script | Purpose |
| --- | --- |
| `R/01_data_prep.R` | Reads the January 2026 facility polygons, calculates density metrics, clips footprints against database-backed land-cover data and exports processed inputs |
| `R/02_eda_existing.R` | Examines existing and planned facilities, produces descriptive tables and figures, and exports the empirical density sample used by the Monte Carlo analysis |
| `R/03_scenarios.R` | Deterministic scenario analysis using a fixed baseline density of 74.5 MWp/km² |
| `R/04_scenarios_montecarlo4.R` | Monte Carlo analysis of land requirements, land-cover allocation and agricultural/grey-area sensitivity |
| `R/01_data_prep_carbon_calc.R` | Prepares a selected facility for import into the greenhouse-gas calculator |
| `R/01_data_prep_carbon_calc_all_facilities.R` | Prepares all facilities, assigns municipality information and exports one feature per facility, optionally in batches |
| `R/envisol_spatial_analysis_updated_tmap.R` | Additional spatial exploration and mapping using external files and a database connection |

The carbon-calculator scripts are a separate workflow; they are not prerequisites for the scenario analysis.

## Requirements

Use R 4.1 or later, as the code uses the native pipe and shorthand anonymous functions. RStudio is useful for the interactive exploratory scripts.

For the main descriptive and scenario analyses, install:

```r
install.packages(c(
  "sf", "dplyr", "tidyr", "readr", "ggplot2",
  "summarytools", "writexl", "forcats", "scales"
))
```

Rebuilding the land-cover inputs with `01_data_prep.R` additionally requires the `NinaR` package and access to the spatial database used by `postgreSQLConnect()` and `clip_gis_layer()`. Obtain the appropriate package installation instructions and database configuration from the project team.

The additional mapping script uses `tidyverse`, `tmap`, `terra`, `DBI`, `RPostgres` and `units`. Spatial packages may require system libraries, depending on the operating system.

The repository does not currently include an `renv` lockfile, so package versions are not pinned.

## Getting started

Clone or download the repository:

Run R with the repository root as the working directory. All main analysis paths are relative to that directory. Create output directories if needed:

```r
dirs <- c("data/processed", "outputs/figures", "outputs/tables")
invisible(lapply(dirs, function(d) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}))
```

### Check the scenario input before running

The scenario CSV must be semicolon-delimited and contain these columns:

- `Year`
- `1 cumulative capacity (MWp)`
- `2 cumulative capacity (MWp)`
- `3 cumulative capacity (MWp)`

### Run from the processed data

Processed capacity-density and land-cover inputs are included, so the descriptive and scenario analyses can be attempted without rebuilding the database-backed spatial inputs.

Run the exploratory analysis interactively:

```r
source("R/02_eda_existing.R")
```

After checking the scenario input path, run the deterministic analysis:

```r
source("R/03_scenarios.R")
```

For the Monte Carlo analysis, first remove or comment out the standalone diagnostic call `to_group()` in `R/04_scenarios_montecarlo4.R`. The function requires a land-cover argument, and that call otherwise stops execution before simulation. Then run:

```r
source("R/04_scenarios_montecarlo4.R")
```

These are intended execution steps, not a claim that the complete workflow has been validated on a clean installation. The scripts include interactive diagnostics and may require further adjustment for the local environment.

### Rebuild the processed spatial inputs

With `NinaR` and database access configured, run:

```r
source("R/01_data_prep.R")
source("R/02_eda_existing.R")
```

The preparation script currently reads `data/raw/sol_jan_2026_final.gpkg`. Review its manual assignment `solar_polygons[12, 7] <- 19` before using a different input version: this correction depends on row and column positions.

Several scripts request the Norwegian locale `no_NB.utf8`. If unavailable on your system, adapt the locale setting and check that Norwegian text and numeric inputs are read correctly.

## Scenario methods and assumptions

The core conversion is:

```text
Land area (km²) = cumulative capacity (MWp) / capacity density (MWp/km²)
```

MWp represents peak capacity; it is distinct from annual electricity production in GWh.

### Deterministic analysis

`03_scenarios.R` uses a fixed capacity density of 74.5 MWp/km². BAU distributes the calculated area using the observed mean land-cover proportions. The alternative allocation places 50% of capacity on agricultural land and 50% on grey areas, with a 1.5-fold area multiplier for agricultural co-use.

### Monte Carlo analysis

`04_scenarios_montecarlo4.R` uses:

| Setting | Current implementation |
| --- | --- |
| Simulations | 5,000 iterations |
| Random seed | 123 |
| Capacity density | Sampling with replacement from `project_density_sample_2026-01.csv` |
| BAU land-cover composition | Dirichlet draws centred on the observed mean proportions |
| Dirichlet concentration | 500 |
| Agricultural share of capacity | 30%, 50% and 70% sensitivity cases |
| Agricultural area multiplier | 1.5 |
| Agricultural classes | Cropland and grassland |
| Grey-area class | Settlements and other artificial areas |
| Capacity trajectories | Fixed low-, medium- and high-growth inputs, restricted to years up to 2050 |
| Summary statistics | Mean, median, 5th percentile and 95th percentile |

The 5th–95th percentile ranges describe variation under the implemented simulation assumptions. They do not capture all sources of uncertainty and should not automatically be interpreted as confidence intervals. In particular, the model does not simulate uncertainty in future capacity trajectories, and the Dirichlet concentration is a specified modelling parameter.

Mean land-cover proportions are calculated by averaging facility-level percentages and then normalising them. They are therefore based on an equal-weighted facility composition, rather than the composition of the total pooled facility area.

The model translates cumulative capacity directly into area. Check whether the selected trajectories represent the relevant ground-mounted capacity before interpreting results. Agricultural/grey-area allocation represents a scenario assumption; it does not establish that sufficient suitable land is available. Agricultural co-use area is also not necessarily equivalent to complete habitat conversion.

## Outputs

The descriptive analysis produces capacity-density and land-cover summaries. The Monte Carlo analysis writes CSV and Excel tables covering:

- Total area requirements by year, growth scenario and allocation.
- Area by individual land-cover class and broader land-cover group.
- Key-year summaries for 2030 and 2050.
- Agricultural and grey-area requirements across allocation sensitivities.

Figures include total-area uncertainty, land-cover-group estimates for 2050, and allocation sensitivity plots. Previously generated outputs are included in the repository; their presence does not establish that they were generated with the current scripts and input versions.

## Greenhouse-gas calculator exports

The carbon-calculator scripts prepare facility footprints for a separate assessment in [NIBIO’s map-based greenhouse-gas calculator](https://klimagasskalkulator.nibio.no/). They export GeoJSON with planning attributes, using `Solkraftanlegg` (purpose code 1531) as the default.

Both scripts require municipality boundaries from a local geodatabase; update `kommune_gdb` to an accessible, appropriate dataset before running. Review the facility selection, purpose code and geometry assumptions in the chosen script.

These exports are analysis inputs, not formal regulatory plans. Assigning a purpose to an entire facility footprint requires substantive review. Successful import alone does not validate the assumptions, and the export scripts do not themselves calculate greenhouse-gas emissions.

## Contributing and reporting issues

Use [GitHub Issues]() to report problems or suggest improvements. Include the script, input version, R/package versions and a reproducible example where possible. Document changes to input data and scenario assumptions alongside code changes.

## Citation and reuse

# Energy Equity & Extreme Heat: Residential Electricity Consumption in New York State

This repository contains the analysis pipeline for a study examining disparities in residential electricity consumption during the cooling season across New York State, with a focus on income, racial segregation, and joint race-income concentration as drivers of energy inequality under extreme heat.

The pipeline is built with the [`targets`](https://docs.ropensci.org/targets/) framework for reproducibility and is part of the [REACH (Research on Energy and Climate Health)](https://reachyale.org) project at Yale University.

---

## Research Overview

**Study period:** 2016–2020 (cooling season: May–September)  
**Geographic units:** ZIP Code Tabulation Areas (ZCTAs) and named communities in New York State  
**Outcome:** Normalized residential electricity consumption (kWh per household or account)

**Key exposures modeled:**

| Measure | Type |
|---|---|
| Median household income (4 categories) | Income |
| ICE Black, ICE Hispanic, ICE Asian | Race-only segregation (Krieger) |
| ICE income-Black, ICE income-Hispanic, ICE income-Asian | Joint race-income (Krieger/Feldman) |
| Heat Index Cooling Degree Days (HICDD) | Climate exposure |

All ICE (Index of Concentration at the Extremes) measures use **households** as the denominator. Joint race-income ICE is defined as white non-Hispanic high-income households (≥$100k) minus the target race's low-income households (<$25k), divided by total households, following [Krieger et al. (2016)](https://doi.org/10.2105/AJPH.2015.302955) and Feldman et al.

**Models:** Linear quantile mixed models (LQMM, τ = 0.5) and linear mixed models (LMER) with random intercepts by county-month grouping and population weights. LQMM models are the primary analysis; LMER models are comparators.

---

## Repository Structure

```
.
├── _targets.R                  # Pipeline definition (all targets)
├── functions/
│   ├── data_aquisition.R       # ACS, energy data, boundary downloads
│   ├── data_processing.R       # Merging, ICE computation, normalization
│   ├── climate_processing.R    # HICDD calculation, climate zone assignment
│   ├── imputation.R            # Dasymetric imputation (building footprints)
│   ├── modeling.R              # LQMM/LMER fitting, results compilation, R²
│   └── outputs.R               # Tables and figures
├── Data/
│   ├── ZIPCodetoZCTACrosswalk2020UDS.xlsx
│   ├── communites_transformed.geojson   # NYC community boundaries
│   └── buildings/                       # Microsoft building footprints (local)
├── daymet/                     # Downloaded climate NetCDF files (auto-populated)
├── Outputs/                    # All results (auto-populated by pipeline)
└── _targets/                   # targets cache (do not edit manually)
```

---

## Requirements

### R version

**R ≥ 4.2** is required. The pipeline was developed and run on **R 4.5.2**.

### Key packages

| Package | Role |
|---|---|
| `targets`, `tarchetypes` | Pipeline orchestration |
| `lqmm` | Linear quantile mixed models |
| `lme4`, `lmerTest` | Linear mixed models |
| `MuMIn` | Marginal/conditional R² (Nakagawa & Schielzeth) |
| `tidycensus` | ACS demographic data |
| `tigris` | TIGER/Line boundary files |
| `daymetr` | Daymet climate data |
| `sf`, `terra`, `exactextractr` | Spatial processing |
| `tidyverse`, `zoo`, `patchwork` | Data wrangling and visualization |

Install all pipeline dependencies at once:

```r
install.packages(c(
  "targets", "tarchetypes", "qs2", "fst",
  "lqmm", "lme4", "lmerTest", "MuMIn",
  "tidycensus", "tigris", "daymetr",
  "sf", "terra", "lwgeom", "exactextractr",
  "tidyverse", "zoo", "patchwork",
  "here", "readxl", "ncdf4", "raster"
))
```

### API credentials

**U.S. Census Bureau API key** (required for ACS data via `tidycensus`):

```r
tidycensus::census_api_key("YOUR_KEY_HERE", install = TRUE)
```

Obtain a free key at [api.census.gov/data/key_signup.html](https://api.census.gov/data/key_signup.html).

**NY Open Data (optional):** Energy data is downloaded from the [NY Open Data portal](https://data.ny.gov). No key is required, but registering for a free Socrata app token will avoid throttling:

```r
# Set as an environment variable or pass directly in data_aquisition.R
Sys.setenv(SOCRATA_APP_TOKEN = "YOUR_TOKEN_HERE")
```

---

## Data Sources

| Source | Description | Access |
|---|---|---|
| [NY Open Data](https://data.ny.gov) | Residential electricity consumption by ZCTA and community, 2016–2020 | Public API |
| [ACS 5-year estimates](https://www.census.gov/programs-surveys/acs) | Demographics (income, race/ethnicity, households) | Census API |
| [TIGER/Line](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html) | ZCTA and county boundaries | `tigris` |
| [Daymet](https://daymet.ornl.gov) | Daily max temperature & vapor pressure (~1 km grid) | `daymetr` |
| [Microsoft Building Footprints](https://github.com/microsoft/USBuildingFootprints) | ~1M building outlines for NY (dasymetric imputation) | Public download |

---

## Running the Pipeline

```r
library(targets)
tar_make()
```

The pipeline will automatically download all remote data on first run. **Note:** The building footprints download and dasymetric imputation step (`energy_zcta_imputed`) are long-running. Once cached, they are frozen with `tar_cue(mode = "never")` and will not rerun unless you explicitly remove that cue.

To inspect the pipeline graph before running:

```r
tar_visnetwork()
```

To read any result without rerunning the pipeline:

```r
tar_read(results_all_models)      # Income model results
tar_read(results_ice_all)         # Race-only ICE results
tar_read(results_ice_income_all)  # Joint race-income ICE results
tar_read(results_lmer_r2)         # LMER R² table
```

---

## Outputs

All outputs are written to `Outputs/` by the pipeline:

| File | Description |
|---|---|
| `table1_demographics.csv` | Demographic summary by dataset (ZCTA, community, combined) |
| `income_model_results.csv` | LQMM + LMER coefficients for income × HICDD interactions |
| `ice_model_results.csv` | LQMM + LMER coefficients for race-only ICE × HICDD interactions |
| `ice_income_model_results.csv` | LQMM + LMER coefficients for joint race-income ICE × HICDD |
| `lmer_r2_values.csv` | Marginal and conditional R² for all LMER models |
| `figure_energy_by_income.png` | Monthly electricity use by income group, 2016–2020 |
| `figure_hicdd_distribution.png` | HICDD distribution and energy response by climate zone |

Each model results file includes six model variants as columns:

| Column | Description |
|---|---|
| Main Model | Combined ZCTA + community dataset with geography control |
| ZCTA Only | ZCTA-level observations only |
| Community Only | Community-level observations only |
| Imputed | ZCTA dataset with dasymetrically imputed missing observations |
| No 2020 | Sensitivity excluding 2020 (COVID-19) |
| LMER | Linear mixed model comparator (combined dataset) |

---

## Climate Exposure Variable

**HICDD (Heat Index Cooling Degree Days)** summarizes heat stress over a month using the [NOAA Heat Index](https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml), computed from Daymet daily maximum temperature and vapor pressure, then summed over days exceeding a 65°F base. New York counties are assigned to three IECC climate zones: Mixed Humid, Cool Humid, and Cold Humid.

---

## Citation

> *Manuscript in preparation.* Please contact the repository maintainer for citation information.

## Contact

Daniel Carrión  
Department of Environmental Health Sciences, Yale School of Public Health  
Yale University  
[daniel.carrion@yale.edu](mailto:daniel.carrion@yale.edu)

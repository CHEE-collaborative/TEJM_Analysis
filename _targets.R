options(timeout = 600)
#requires the qs2 and fst packages

library(targets)
library(tarchetypes)

# Source all function files
source("./functions/climate_processing.R")
source("./functions/data_aquisition.R")
source("./functions/data_processing.R")
source("./functions/imputation.R")
source("./functions/modeling.R")
source("./functions/outputs.R")

# Set options
tar_option_set(
  packages = c(
    "here",
    "lme4",
    "lmerTest",
    "lqmm",
    "MuMIn",
    "tidyverse",
    "tidycensus",
    "tigris",
    "terra",
    "sf",
    "lwgeom",
    "daymetr",
    "ncdf4",
    "raster",
    "readxl",
    "rnaturalearth",
    "geodata",
    "exactextractr",
    "zoo",
    "patchwork"
  ),
  format = 'qs',  # Change default to qs
  workspace_on_error = TRUE,
  memory = "transient",  # Clean up memory between targets
  garbage_collection = TRUE  # Force garbage collection
)

#Define the pipeline
list(
  #Configuration
  tar_target(
    analysis_config,
    list(
      years = c(2016:2020),
      months = c(5:9),
      income_breaks = c(0, 50000, 70000, 90000, Inf),
      income_labels = c("<=50k", "50k-70k", "70k-90k", ">90k"),
      quantile = 0.5,
      scale_factor = 245570
    ),
    format = "rds"
  ),

  # Basic data acquisition
  tar_target(
    energy_data_zcta_raw,
    download_zcta_energy_data(),
    format = "fst"
  ),

  tar_target(
    energy_data_community_raw,
    download_community_energy_data(),
    format = "fst"
  ),

  # Get the administrative boundary polygons
  tar_target(
    zcta_boundaries,
    get_zcta_boundaries(year = 2010, state = "New York")
  ),

  tar_target(
    community_boundaries,
    get_community_boundaries()
  ),

  tar_target(
    county_boundaries,
    get_county_boundaries(state = "36", year = 2020)
  ),

  # Get the corresponding demographics
  tar_target(
    demographics_zcta,
    get_acs_demographics("zcta", c(2016:2020))
  ),

  tar_target(
    demographics_place,
    get_acs_demographics("place", c(2016:2020))
  ),

  tar_target(
    demographics_subdivision,
    get_acs_demographics("county subdivision", c(2016:2020))
  ),

  # Construct NYS climate zones
  tar_target(
    climate_zones,
    assign_climate_zones(county_boundaries)
  ),

  # Match geographies 
  tar_target(
    zcta_county_match,
    match_zcta_to_county(zcta_boundaries, county_boundaries, climate_zones)
  ),

  tar_target(
    community_county_match,
    match_communities_to_county(
      bind_rows(demographics_place, demographics_subdivision),
      county_boundaries,
      climate_zones
    )
  ),

  # Access and process temperature data
  tar_target(
    daymet_data,
    download_daymet_data(
      years = analysis_config$years,
      bbox = c(46, -80, 40, -71)
    ),
    format = "file"
  ),

  tar_target(
    hicdd_zcta,
    calculate_hicdd(
      daymet_data,
      census_data = zcta_boundaries,
      months = analysis_config$months
    )
  ),

  tar_target(
    hicdd_community,
    calculate_hicdd(
      daymet_data,
      census_data = community_boundaries,
      months = analysis_config$months
    )
  ),

  # Access and process energy data
  tar_target(
    energy_zcta_processed,
    process_zcta_energy(
      energy_data_zcta_raw,
      demographics_zcta,
      hicdd_zcta,
      zcta_county_match,
      analysis_config
    )
  ),

  tar_target(
    energy_community_processed,
    process_community_energy(
      energy_raw = energy_data_community_raw,
      demographics = bind_rows(demographics_place, demographics_subdivision),
      hicdd = hicdd_community,
      community_county_match = community_county_match, 
      config = analysis_config
    )
  ),

  # Create the datasets for the analyses
  tar_target(
    dataset_zcta_only,
    prepare_zcta_dataset(energy_zcta_processed)
  ),

  tar_target(
    dataset_community_only,
    prepare_community_dataset(energy_community_processed)
  ),

  tar_target(
    dataset_combined,
    create_combined_dataset(
      energy_zcta_processed,
      energy_community_processed,
      zcta_boundaries,
      community_boundaries
    )
  ),
  
  tar_target(
    dataset_no_2020,
    dataset_combined %>% filter(year != 2020)
  ),
  ## Get data and construct an imputation model to get a uniform dataset at the ZCTA level
  # NOTE: These 3 targets are frozen (cue = "never") to avoid rerunning expensive
  # dasymetric imputation. The downstream prepare_imputed_dataset() backfills any
  # missing demographic columns from the updated demographics. Remove cue = "never"
  # if the imputation itself needs to be refreshed.
  tar_target(
    building_footprints,
    download_building_footprints(),
    format = "qs",
    cue = tar_cue(mode = "never")
  ),

  tar_target(
    buildings_with_height,
    process_buildings_with_height(building_footprints),
    format = "qs",
    cue = tar_cue(mode = "never")
  ),

  # Imputed ZCTA energy
  tar_target(
    energy_zcta_imputed,
    impute_missing_zcta_energy(
      energy_zcta_processed,
      energy_community_processed,
      buildings_with_height,
      hicdd_zcta
    ),
    format = "qs",
    cue = tar_cue(mode = "never")
  ),
  
  # Create the imputed dataset (backfills missing income columns from demographics)
  tar_target(
    dataset_imputed,
    prepare_imputed_dataset(energy_zcta_imputed, demographics_zcta)
  ),
  
  # Create tables of demographics 
  tar_target(
    table_demographics,
    create_demographic_table(
      list(
        zcta = dataset_zcta_only,
        community = dataset_community_only,
        combined = dataset_combined
      )
    )
  ),

  # FIT THE STATISTICAL MODELS FOR INCOME 
  tar_target(
    model_income_combined,
    fit_income_model(
      data = dataset_combined,
      geography_control = TRUE
    )
  ),

  tar_target(
    model_income_zcta,
    fit_income_model(
      data = dataset_zcta_only,
      geography_control = FALSE
    )
  ),

  tar_target(
    model_income_community,
    fit_income_model(
      data = dataset_community_only,
      geography_control = FALSE
    )
  ),
  
  tar_target(
    model_income_no_2020,
    fit_income_model(
      data = dataset_no_2020,
      geography_control = TRUE
    )
  ),

  tar_target(
    model_income_lmer,
    fit_income_model_lmer(
      data = dataset_combined
    )
  ),
  
  tar_target(
    model_income_imputed,
    fit_income_model(
      data = dataset_imputed,
      geography_control = FALSE
    )
  ),
  
  # Compile income results
  tar_target(
    results_all_models,
    compile_model_results(
      list(
        main = model_income_combined,
        zcta_only = model_income_zcta,
        community_only = model_income_community,
        imputed = model_income_imputed,
        no_2020 = model_income_no_2020,
        lmer = model_income_lmer
      ),
      model_type = "income",
      scale_factor = analysis_config$scale_factor
    )
  ),
  
  # FIT THE STATISTICAL MODELS FOR ICE
  
  # Main combined dataset ICE models
  tar_target(
    model_ice_black_combined,
    fit_ice_model(
      data = dataset_combined,
      ice_variable = "ICE_black",
      geography_control = TRUE
    )
  ),
  
  tar_target(
    model_ice_hispanic_combined,
    fit_ice_model(
      data = dataset_combined,
      ice_variable = "ICE_hisp",
      geography_control = TRUE
    )
  ),
  
  tar_target(
    model_ice_asian_combined,
    fit_ice_model(
      data = dataset_combined,
      ice_variable = "ICE_asian",
      geography_control = TRUE
    )
  ),
  
  # ZCTA-only ICE models
  tar_target(
    model_ice_black_zcta,
    fit_ice_model(
      data = dataset_zcta_only,
      ice_variable = "ICE_black",
      geography_control = FALSE
    )
  ),
  
  tar_target(
    model_ice_hispanic_zcta,
    fit_ice_model(
      data = dataset_zcta_only,
      ice_variable = "ICE_hisp",
      geography_control = FALSE
    )
  ),
  
  tar_target(
    model_ice_asian_zcta,
    fit_ice_model(
      data = dataset_zcta_only,
      ice_variable = "ICE_asian",
      geography_control = FALSE
    )
  ),
  
  # Community-only ICE models
  tar_target(
    model_ice_black_community,
    fit_ice_model(
      data = dataset_community_only,
      ice_variable = "ICE_black",
      geography_control = FALSE
    )
  ),
  
  tar_target(
    model_ice_hispanic_community,
    fit_ice_model(
      data = dataset_community_only,
      ice_variable = "ICE_hisp",
      geography_control = FALSE
    )
  ),
  
  tar_target(
    model_ice_asian_community,
    fit_ice_model(
      data = dataset_community_only,
      ice_variable = "ICE_asian",
      geography_control = FALSE
    )
  ),
  
  # No-2020 ICE models
  tar_target(
    model_ice_black_no_2020,
    fit_ice_model(
      data = dataset_no_2020,
      ice_variable = "ICE_black",
      geography_control = FALSE
    )
  ),
  
  tar_target(
    model_ice_hispanic_no_2020,
    fit_ice_model(
      data = dataset_no_2020,
      ice_variable = "ICE_hisp",
      geography_control = FALSE
    )
  ),
  
  tar_target(
    model_ice_asian_no_2020,
    fit_ice_model(
      data = dataset_no_2020,
      ice_variable = "ICE_asian",
      geography_control = FALSE
    )
  ),
  
  # Imputed dataset ICE models
  tar_target(
    model_ice_black_imputed,
    fit_ice_model(
      data = dataset_imputed,
      ice_variable = "ICE_black",
      geography_control = FALSE
    )
  ),
  
  tar_target(
    model_ice_hispanic_imputed,
    fit_ice_model(
      data = dataset_imputed,
      ice_variable = "ICE_hisp",
      geography_control = FALSE
    )
  ),
  
  tar_target(
    model_ice_asian_imputed,
    fit_ice_model(
      data = dataset_imputed,
      ice_variable = "ICE_asian",
      geography_control = FALSE
    )
  ),
  
  # LMER ICE models
  tar_target(
    model_ice_black_lmer,
    fit_ice_model_lmer(
      data = dataset_combined,
      ice_variable = "ICE_black"
    )
  ),

  tar_target(
    model_ice_hispanic_lmer,
    fit_ice_model_lmer(
      data = dataset_combined,
      ice_variable = "ICE_hisp"
    )
  ),

  tar_target(
    model_ice_asian_lmer,
    fit_ice_model_lmer(
      data = dataset_combined,
      ice_variable = "ICE_asian"
    )
  ),

  # FIT THE STATISTICAL MODELS FOR JOINT RACE-INCOME ICE (Krieger/Feldman)

  # Main combined dataset joint ICE models
  tar_target(
    model_ice_income_black_combined,
    fit_ice_model(
      data = dataset_combined,
      ice_variable = "ICE_income_Black",
      geography_control = TRUE
    )
  ),

  tar_target(
    model_ice_income_hispanic_combined,
    fit_ice_model(
      data = dataset_combined,
      ice_variable = "ICE_income_Hispanic",
      geography_control = TRUE
    )
  ),

  tar_target(
    model_ice_income_asian_combined,
    fit_ice_model(
      data = dataset_combined,
      ice_variable = "ICE_income_Asian",
      geography_control = TRUE
    )
  ),

  # ZCTA-only joint ICE models
  tar_target(
    model_ice_income_black_zcta,
    fit_ice_model(
      data = dataset_zcta_only,
      ice_variable = "ICE_income_Black",
      geography_control = FALSE
    )
  ),

  tar_target(
    model_ice_income_hispanic_zcta,
    fit_ice_model(
      data = dataset_zcta_only,
      ice_variable = "ICE_income_Hispanic",
      geography_control = FALSE
    )
  ),

  tar_target(
    model_ice_income_asian_zcta,
    fit_ice_model(
      data = dataset_zcta_only,
      ice_variable = "ICE_income_Asian",
      geography_control = FALSE
    )
  ),

  # Community-only joint ICE models
  tar_target(
    model_ice_income_black_community,
    fit_ice_model(
      data = dataset_community_only,
      ice_variable = "ICE_income_Black",
      geography_control = FALSE
    )
  ),

  tar_target(
    model_ice_income_hispanic_community,
    fit_ice_model(
      data = dataset_community_only,
      ice_variable = "ICE_income_Hispanic",
      geography_control = FALSE
    )
  ),

  tar_target(
    model_ice_income_asian_community,
    fit_ice_model(
      data = dataset_community_only,
      ice_variable = "ICE_income_Asian",
      geography_control = FALSE
    )
  ),

  # No-2020 joint ICE models
  tar_target(
    model_ice_income_black_no_2020,
    fit_ice_model(
      data = dataset_no_2020,
      ice_variable = "ICE_income_Black",
      geography_control = TRUE
    )
  ),

  tar_target(
    model_ice_income_hispanic_no_2020,
    fit_ice_model(
      data = dataset_no_2020,
      ice_variable = "ICE_income_Hispanic",
      geography_control = TRUE
    )
  ),

  tar_target(
    model_ice_income_asian_no_2020,
    fit_ice_model(
      data = dataset_no_2020,
      ice_variable = "ICE_income_Asian",
      geography_control = TRUE
    )
  ),

  # Imputed dataset joint ICE models
  tar_target(
    model_ice_income_black_imputed,
    fit_ice_model(
      data = dataset_imputed,
      ice_variable = "ICE_income_Black",
      geography_control = FALSE
    )
  ),

  tar_target(
    model_ice_income_hispanic_imputed,
    fit_ice_model(
      data = dataset_imputed,
      ice_variable = "ICE_income_Hispanic",
      geography_control = FALSE
    )
  ),

  tar_target(
    model_ice_income_asian_imputed,
    fit_ice_model(
      data = dataset_imputed,
      ice_variable = "ICE_income_Asian",
      geography_control = FALSE
    )
  ),

  # LMER joint ICE models
  tar_target(
    model_ice_income_black_lmer,
    fit_ice_model_lmer(
      data = dataset_combined,
      ice_variable = "ICE_income_Black"
    )
  ),

  tar_target(
    model_ice_income_hispanic_lmer,
    fit_ice_model_lmer(
      data = dataset_combined,
      ice_variable = "ICE_income_Hispanic"
    )
  ),

  tar_target(
    model_ice_income_asian_lmer,
    fit_ice_model_lmer(
      data = dataset_combined,
      ice_variable = "ICE_income_Asian"
    )
  ),
  
  # Compile all ICE results
  tar_target(
    results_ice_all,
    bind_rows(
      compile_model_results(
        list(
          main = model_ice_black_combined,
          zcta_only = model_ice_black_zcta,
          community_only = model_ice_black_community,
          imputed = model_ice_black_imputed,
          no_2020 = model_ice_black_no_2020,
          lmer = model_ice_black_lmer
        ),
        model_type = "ice",
        scale_factor = analysis_config$scale_factor
      ) %>% mutate(ICE_type = "Black"),
      
      compile_model_results(
        list(
          main = model_ice_hispanic_combined,
          zcta_only = model_ice_hispanic_zcta,
          community_only = model_ice_hispanic_community,
          imputed = model_ice_hispanic_imputed,
          no_2020 = model_ice_hispanic_no_2020,
          lmer = model_ice_hispanic_lmer
        ),
        model_type = "ice",
        scale_factor = analysis_config$scale_factor
      ) %>% mutate(ICE_type = "Hispanic"),
      
      compile_model_results(
        list(
          main = model_ice_asian_combined,
          zcta_only = model_ice_asian_zcta,
          community_only = model_ice_asian_community,
          imputed = model_ice_asian_imputed,
          no_2020 = model_ice_asian_no_2020,
          lmer = model_ice_asian_lmer
        ),
        model_type = "ice",
        scale_factor = analysis_config$scale_factor
      ) %>% mutate(ICE_type = "Asian")
    )
  ),
  
  # Compile all joint race-income ICE results
  tar_target(
    results_ice_income_all,
    bind_rows(
      compile_model_results(
        list(
          main = model_ice_income_black_combined,
          zcta_only = model_ice_income_black_zcta,
          community_only = model_ice_income_black_community,
          imputed = model_ice_income_black_imputed,
          no_2020 = model_ice_income_black_no_2020,
          lmer = model_ice_income_black_lmer
        ),
        model_type = "ice_income",
        scale_factor = analysis_config$scale_factor
      ) %>% mutate(ICE_type = "Black"),

      compile_model_results(
        list(
          main = model_ice_income_hispanic_combined,
          zcta_only = model_ice_income_hispanic_zcta,
          community_only = model_ice_income_hispanic_community,
          imputed = model_ice_income_hispanic_imputed,
          no_2020 = model_ice_income_hispanic_no_2020,
          lmer = model_ice_income_hispanic_lmer
        ),
        model_type = "ice_income",
        scale_factor = analysis_config$scale_factor
      ) %>% mutate(ICE_type = "Hispanic"),

      compile_model_results(
        list(
          main = model_ice_income_asian_combined,
          zcta_only = model_ice_income_asian_zcta,
          community_only = model_ice_income_asian_community,
          imputed = model_ice_income_asian_imputed,
          no_2020 = model_ice_income_asian_no_2020,
          lmer = model_ice_income_asian_lmer
        ),
        model_type = "ice_income",
        scale_factor = analysis_config$scale_factor
      ) %>% mutate(ICE_type = "Asian")
    )
  ),

  # Create figures
  tar_target(
    figure_energy_by_income,
    plot_energy_by_income(dataset_combined, analysis_config)
  ),
  
  tar_target(
    figure_hicdd_distribution,
    plot_hicdd_distribution(dataset_combined, climate_zones, analysis_config),
    format = "rds"
  ),
  # Save outputs
  tar_target(
    save_demographics,
    {
      if (!dir.exists(here("Outputs"))) dir.create(here("Outputs"))
      write_csv(table_demographics, here("Outputs", "table1_demographics.csv"))
      TRUE
    }
  ),

  tar_target(
    save_results_income,
    {
      if (!dir.exists(here("Outputs"))) dir.create(here("Outputs"))
      write_csv(results_all_models, here("Outputs", "income_model_results.csv"))  
      TRUE
    }
  ),
  
  tar_target(
    save_results_ice,
    {
      if (!dir.exists(here("Outputs"))) dir.create(here("Outputs"))
      write_csv(results_ice_all, here("Outputs", "ice_model_results.csv"))
      TRUE
    }
  ),

  tar_target(
    save_results_ice_income,
    {
      if (!dir.exists(here("Outputs"))) dir.create(here("Outputs"))
      write_csv(results_ice_income_all, here("Outputs", "ice_income_model_results.csv"))
      TRUE
    }
  ),
  
  # Save figures
  tar_target(
    save_figures,
    {
      if (!dir.exists(here("Outputs"))) dir.create(here("Outputs"))
      
      # Save energy by income figure
      ggsave(
        filename = here("Outputs", "figure_energy_by_income.png"),
        plot = figure_energy_by_income,
        width = 16,
        height = 8,
        dpi = 300
      )
      
      # Save HICDD distribution figure
      ggsave(
        filename = here("Outputs", "figure_hicdd_distribution.png"),
        plot = figure_hicdd_distribution,
        width = 14,
        height = 10,
        dpi = 300
      )
      
      TRUE
    }
  )
)

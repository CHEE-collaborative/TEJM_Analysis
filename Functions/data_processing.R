# Data Processing Functions
# Functions for merging and processing data

#' Process ZCTA energy data with demographics and climate
#' @param energy_raw Raw ZCTA energy data
#' @param demographics ZCTA demographic data
#' @param hicdd HICDD climate data
#' @param zcta_county_match ZCTA to county matching
#' @param config Analysis configuration
#' @return Processed ZCTA dataset
process_zcta_energy <- function(energy_raw, demographics, hicdd, zcta_county_match, config) {
  message("Processing ZCTA energy data...")
  
  # Rename ZCTA column for consistency
  energy_clean <- energy_raw %>%
    rename(GEOID = ZCTA) %>%
    mutate(
      year = as.numeric(year),
      month = as.numeric(month)
    )
  
  # Process demographics - interpolate missing income values
  demographics_processed <- demographics %>%
    st_drop_geometry() %>%
    group_by(GEOID) %>%
    arrange(year) %>%
    mutate(
      Median_Income_Imputed = if(all(is.na(Median_Income_Household))) {
        NA_real_
      } else {
        zoo::na.approx(Median_Income_Household, year, rule = 2, na.rm = FALSE)
      },
      Avg_Household_Size = if_else(
        is.na(Avg_Household_Size),
        zoo::na.approx(Avg_Household_Size, year, rule = 2, na.rm = FALSE),
        Avg_Household_Size)
    ) %>%
    ungroup()
  
  # Process HICDD data
  hicdd_clean <- hicdd %>%
    rename(HICDD = mean) %>%
    dplyr::select(GEOID, year, month, HICDD)
  
  # Merge all data sources
  merged_data <- energy_clean %>%
    left_join(hicdd_clean, by = c("GEOID", "year", "month")) %>%
    left_join(demographics_processed, by = c("GEOID", "year")) %>%
    left_join(zcta_county_match, by = "GEOID")
  
  # Add calculated fields
  final_data <- merged_data %>%
    mutate(
      # Create grouping variables
      groups = paste0(year, "_", month),
      groups_county = paste0(groups, "_", county_fips),
      groups_climate = paste0(groups, "_", Climate_Region),
      
      # Income categories
      Median_Income_cat = cut(
        Median_Income_Imputed,
        breaks = config$income_breaks,
        labels = config$income_labels,
        include.lowest = TRUE
      ),
      
      # Normalize energy
      Normalized_Energy = if_else(
        is.na(total_accounts) | total_accounts == 0,
        total_energy / Households,
        total_energy / total_accounts
      ),
      
      # Race/ethnicity proportions
      White_prop = White / Total_pop,
      Black_prop = Black / Total_pop,
      Asian_prop = Asian / Total_pop,
      Hispanic_prop = Hispanic / Total_pop,
      
      # Aggregate race-specific income bins
      White_household_income_high = Household_Income_White_high1 + Household_Income_White_high2 +
        Household_Income_White_high3 + Household_Income_White_high4,
      Black_household_income_low = Household_Income_Black_low1 + Household_Income_Black_low2 +
        Household_Income_Black_low3 + Household_Income_Black_low4,
      Hispanic_household_income_low = Household_Income_Hispanic_low1 + Household_Income_Hispanic_low2 +
        Household_Income_Hispanic_low3 + Household_Income_Hispanic_low4,
      Asian_household_income_low = Household_Income_Asian_low1 + Household_Income_Asian_low2 +
        Household_Income_Asian_low3 + Household_Income_Asian_low4,

      # Race-only ICE indices
      ICE_black = (White - Black) / Households,
      ICE_hisp = (White - Hispanic) / Households,
      ICE_asian = (White - Asian) / Households,

      # Joint race-income ICE (Krieger/Feldman)
      ICE_income_Black = (White_household_income_high - Black_household_income_low) / Households,
      ICE_income_Hispanic = (White_household_income_high - Hispanic_household_income_low) / Households,
      ICE_income_Asian = (White_household_income_high - Asian_household_income_low) / Households,

      # Flag for actual vs imputed data
      True_Energy = total_energy,
      is_imputed = FALSE
    ) %>%
    filter(!is.na(Normalized_Energy), Normalized_Energy > 0)
  
  message("  Processed ", nrow(final_data), " ZCTA-month observations")
  
  return(final_data)
}

#' Process community energy data with demographics and climate
#' @param energy_raw Raw community energy data
#' @param demographics Community demographic data (places and subdivisions)
#' @param hicdd HICDD climate data
#' @param community_county_match Community to county matching
#' @param config Analysis configuration
#' @return Processed community dataset
process_community_energy <- function(energy_raw, demographics, hicdd, community_county_match, config) {
  message("Processing community energy data...")
  
  # Clean energy data
  energy_clean <- energy_raw %>%
    mutate(
      GEOID = FULL_FIPS,
      year = as.numeric(year),
      month = as.numeric(month),
      True_Energy = as.numeric(value),
      total_accounts = as.numeric(number_of_accounts)
    ) %>%
    dplyr::select(GEOID, com_name, county_name, com_type, year, month, True_Energy, total_accounts)
  
  # Process demographics
  demographics_processed <- demographics %>%
    st_drop_geometry() %>%
    group_by(GEOID) %>%
    arrange(year) %>%
    mutate(
      Median_Income_Imputed = if_else(
        is.na(Median_Income_Household),
        zoo::na.approx(Median_Income_Household, year, rule = 2, na.rm = FALSE),
        Median_Income_Household),
      Avg_Household_Size = if_else(
        is.na(Avg_Household_Size),
        Size_per_House,
        Avg_Household_Size)
    ) %>%
    ungroup()
  
  # Process HICDD
  hicdd_clean <- hicdd %>%
    rename(HICDD = mean) %>%
    dplyr::select(GEOID, year, month, HICDD)
  
  # Merge all data
  merged_data <- energy_clean %>%
    left_join(hicdd_clean, by = c("GEOID", "year", "month")) %>%
    left_join(demographics_processed, by = c("GEOID", "year")) %>%
    left_join(community_county_match, by = "GEOID")
  
  # Add calculated fields
  final_data <- merged_data %>%
    mutate(
      # Create grouping variables
      groups = paste0(year, "_", month),
      groups_county = paste0(groups, "_", county_fips),
      groups_climate = paste0(groups, "_", Climate_Region),
      
      # Income categories
      Median_Income_cat = cut(
        Median_Income_Imputed,
        breaks = config$income_breaks,
        labels = config$income_labels,
        include.lowest = TRUE
      ),
      
      # Handle missing account data
      total_accounts = if_else(total_accounts == 0, NA_real_, total_accounts)
    ) %>%
    group_by(GEOID) %>%
    arrange(year, month) %>%
    fill(total_accounts, .direction = "downup") %>%
    ungroup() %>%
    mutate(
      # Normalize energy
      Normalized_Energy = if_else(
        is.na(total_accounts),
        True_Energy / Households,
        True_Energy / total_accounts
      ),
      
      # Race/ethnicity proportions
      White_prop = White / Total_pop,
      Black_prop = Black / Total_pop,
      Asian_prop = Asian / Total_pop,
      Hispanic_prop = Hispanic / Total_pop,
      
      # Aggregate race-specific income bins
      White_household_income_high = Household_Income_White_high1 + Household_Income_White_high2 +
        Household_Income_White_high3 + Household_Income_White_high4,
      Black_household_income_low = Household_Income_Black_low1 + Household_Income_Black_low2 +
        Household_Income_Black_low3 + Household_Income_Black_low4,
      Hispanic_household_income_low = Household_Income_Hispanic_low1 + Household_Income_Hispanic_low2 +
        Household_Income_Hispanic_low3 + Household_Income_Hispanic_low4,
      Asian_household_income_low = Household_Income_Asian_low1 + Household_Income_Asian_low2 +
        Household_Income_Asian_low3 + Household_Income_Asian_low4,

      # Race-only ICE indices
      ICE_black = (White - Black) / Households,
      ICE_hisp = (White - Hispanic) / Households,
      ICE_asian = (White - Asian) / Households,

      # Joint race-income ICE (Krieger/Feldman)
      ICE_income_Black = (White_household_income_high - Black_household_income_low) / Households,
      ICE_income_Hispanic = (White_household_income_high - Hispanic_household_income_low) / Households,
      ICE_income_Asian = (White_household_income_high - Asian_household_income_low) / Households
    ) %>%
    filter(!is.na(Normalized_Energy), Normalized_Energy > 0)
  
  message("  Processed ", nrow(final_data), " community-month observations")
  
  return(final_data)
}

#' Create combined dataset with non-overlapping ZCTAs and communities
#' @param zcta_data Processed ZCTA data
#' @param community_data Processed community data
#' @param zcta_boundaries ZCTA spatial boundaries
#' @param community_boundaries Community spatial boundaries
#' @return Combined dataset
create_combined_dataset <- function(zcta_data, community_data, zcta_boundaries, community_boundaries) {
  message("Creating combined non-overlapping dataset...")
  
  # Find overlapping communities
  zctas_with_energy <- zcta_boundaries %>%
    filter(GEOID %in% unique(zcta_data$GEOID))
  
  communities_with_energy <- community_boundaries %>%
    filter(GEOID %in% unique(community_data$GEOID))
  
  # Find communities that overlap with ZCTAs
  overlapping_communities <- communities_with_energy %>%
    st_buffer(0) %>%  # Fix any geometry issues
    st_filter(zctas_with_energy, .predicate = st_intersects) %>%
    pull(GEOID)
  
  # Keep non-overlapping communities
  community_data_filtered <- community_data %>%
    filter(!GEOID %in% overlapping_communities) %>%
    mutate(geography = "community")
  
  # Add geography flag to ZCTA data
  zcta_data_filtered <- zcta_data %>%
    filter(!is.na(True_Energy)) %>%
    mutate(geography = "ZCTA")
  
  # Get common columns
  common_cols <- intersect(names(zcta_data_filtered), names(community_data_filtered))
  
  # Combine datasets
  combined <- bind_rows(
    dplyr::select(zcta_data_filtered, all_of(common_cols)),
    dplyr::select(community_data_filtered, all_of(common_cols))) %>% 
    filter(Total_pop>0 & !is.na(Total_pop)) %>%
    filter(!is.na(Median_Income_cat))
  
  message("  Combined dataset: ", 
          sum(combined$geography == "ZCTA"), " ZCTA observations, ",
          sum(combined$geography == "community"), " community observations")
  
  return(combined)
}

#' Prepare ZCTA-only dataset for analysis
#' @param zcta_data Processed ZCTA data
#' @return ZCTA dataset ready for modeling
prepare_zcta_dataset <- function(zcta_data) {
  zcta_data %>%
    filter(!is.na(True_Energy)) %>%
    filter(!is.na(Normalized_Energy)) %>%
    filter(!is.na(Median_Income_cat)) %>%
    filter(!is.na(HICDD)) %>%
    filter(Households > 0)
}

#' Prepare community-only dataset for analysis
#' @param community_data Processed community data
#' @return Community dataset ready for modeling
prepare_community_dataset <- function(community_data) {
  community_data %>%
    filter(!is.na(Normalized_Energy)) %>%
    filter(!is.na(Median_Income_cat)) %>%
    filter(!is.na(HICDD)) %>%
    filter(Households > 0)
}

#' Prepare dataset with imputed values included
#' @param zcta_imputed ZCTA data with imputed values
#' @param demographics ZCTA demographic data (used to backfill income columns
#'   when the cached imputation output predates the joint ICE variables)
#' @return Dataset including imputed values
prepare_imputed_dataset <- function(zcta_imputed, demographics) {

  # Backfill race-specific income columns if missing from cached imputation
  needs_income_cols <- !("Household_Income_White_high1" %in% names(zcta_imputed))

  if (needs_income_cols) {
    message("  Backfilling income bin columns from demographics...")
    income_cols <- c(
      "Household_Income_White_high1", "Household_Income_White_high2",
      "Household_Income_White_high3", "Household_Income_White_high4",
      "Household_Income_Black_low1", "Household_Income_Black_low2",
      "Household_Income_Black_low3", "Household_Income_Black_low4",
      "Household_Income_Hispanic_low1", "Household_Income_Hispanic_low2",
      "Household_Income_Hispanic_low3", "Household_Income_Hispanic_low4",
      "Household_Income_Asian_low1", "Household_Income_Asian_low2",
      "Household_Income_Asian_low3", "Household_Income_Asian_low4"
    )

    demo_income <- demographics %>%
      st_drop_geometry() %>%
      dplyr::select(GEOID, year, any_of(income_cols))

    zcta_imputed <- zcta_imputed %>%
      left_join(demo_income, by = c("GEOID", "year"))
  }

  zcta_imputed %>%
    mutate(
      # Aggregate race-specific income bins
      White_household_income_high = Household_Income_White_high1 + Household_Income_White_high2 +
        Household_Income_White_high3 + Household_Income_White_high4,
      Black_household_income_low = Household_Income_Black_low1 + Household_Income_Black_low2 +
        Household_Income_Black_low3 + Household_Income_Black_low4,
      Hispanic_household_income_low = Household_Income_Hispanic_low1 + Household_Income_Hispanic_low2 +
        Household_Income_Hispanic_low3 + Household_Income_Hispanic_low4,
      Asian_household_income_low = Household_Income_Asian_low1 + Household_Income_Asian_low2 +
        Household_Income_Asian_low3 + Household_Income_Asian_low4,

      # Recompute all ICE with Households denominator
      ICE_black = (White - Black) / Households,
      ICE_hisp = (White - Hispanic) / Households,
      ICE_asian = (White - Asian) / Households,

      # Joint race-income ICE (Krieger/Feldman)
      ICE_income_Black = (White_household_income_high - Black_household_income_low) / Households,
      ICE_income_Hispanic = (White_household_income_high - Hispanic_household_income_low) / Households,
      ICE_income_Asian = (White_household_income_high - Asian_household_income_low) / Households
    ) %>%
    filter(!is.na(Normalized_Energy)) %>%
    filter(!is.na(Median_Income_cat)) %>%
    filter(!is.na(HICDD)) %>%
    filter(Households > 0) %>%
    mutate(
      # Add a flag to track imputed vs actual
      data_type = if_else(is_imputed, "Imputed", "Actual")
    )
}

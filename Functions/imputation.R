# Building-based Imputation Functions
# Functions for dasymetric mapping using building footprints

#' Process buildings with height categories
#' @param buildings_data Raw building footprints
#' @return Buildings with height categories
process_buildings_with_height <- function(buildings_data) {
  message("Processing building heights...")
  
  # Load population raster for filtering
  pop_raster <- download_population_raster()
  
  sf_use_s2(FALSE)
  # Filter buildings to residential areas
  buildings_residential <- filter_residential_buildings(buildings_data, pop_raster) 
  
  # Load height categories from block groups
  height_data <- load_height_categories()
  
  # Assign heights to buildings
  buildings_with_height <- assign_building_heights(buildings_residential, height_data)
  
  # Add occupancy ratios
  occupancy_data <- get_occupancy_ratios()
  buildings_with_ratios <- st_join(buildings_with_height, occupancy_data, join = st_within)
  
  # Calculate effective area
  buildings_final <- buildings_with_ratios %>%
    mutate(
      occupied_ratio = if_else(is.na(occupied_ratio), 1, occupied_ratio),
      effective_area = area * occupied_ratio
    )
  
  message("  Processed ", format(nrow(buildings_final), big.mark = ","), 
          " residential buildings")
  
  return(buildings_final)
}

#' Download population raster for filtering
#' @return Population raster
download_population_raster <- function() {
  message("  Processing population raster data...")
  
  pop_path <- here("Data", "population")
  if (!dir.exists(pop_path)) {
    dir.create(pop_path, recursive = TRUE)
  }
  
  # Check if merged raster already exists
  merged_file <- file.path(pop_path, "merged_population.tif")
  if (file.exists(merged_file)) {
    message("    Loading existing merged population raster...")
    return(terra::rast(merged_file))
  }
  
  # Download GHSL data
  url <- "https://springernature.figshare.com/ndownloader/files/35940383"
  destfile <- file.path(pop_path, "zip_file.zip")
  
  if (!file.exists(destfile)) {
    message("    Downloading population data (this may take a while)...")
    download.file(url, destfile, mode = "wb")
  }
  
  # Extract files
  unzip_dir <- file.path(pop_path, "unzipped_files")
  if (!dir.exists(file.path(unzip_dir, "tifs"))) {
    message("    Extracting population data...")
    unzip(destfile, exdir = unzip_dir)
  }
  
  # Read tif data of population density 30m
  file_path <- file.path(unzip_dir, "tifs")
  files <- list.files(path = file_path, pattern = "*", all.files = FALSE, full.names = FALSE)
  
  # Filter for New York State (FIPS code 36)
  files_NY <- files[which(substr(files, 11, 12) == "36")]
  
  message("    Reading ", length(files_NY), " population tiles for New York...")
  pop_data_list <- lapply(file.path(file_path, files_NY), terra::rast)
  
  # Merge all tiles into a single raster
  message("    Merging population tiles...")
  if (length(pop_data_list) > 1) {
    merged_raster <- do.call(terra::merge, pop_data_list)
  } else {
    merged_raster <- pop_data_list[[1]]
  }
  
  # Save for future use
  writeRaster(merged_raster, merged_file, overwrite = TRUE)
  
  message("    Population raster ready")
  return(merged_raster)
}

#' Filter buildings to residential areas
#' @param buildings Building footprints
#' @param pop_raster Population raster
#' @return Filtered buildings
filter_residential_buildings <- function(buildings, pop_raster) {
  message("  Filtering buildings to residential areas...")
  
  if (is.null(pop_raster)) {
    warning("No population raster available, returning all buildings")
    buildings$population <- 1  # Assume all buildings are residential
    return(buildings)
  }
  
  # Get building centroids (suppress warnings about lon/lat)
  building_centroids <- suppressWarnings(st_centroid(buildings))
  
  # Ensure same CRS - project raster to match buildings
  pop_raster_proj <- terra::project(pop_raster, crs(buildings))
  
  # Use exact_extract for polygon extraction
  pop_values <- exact_extract(
    pop_raster_proj, 
    buildings, 
    fun = function(values, coverage_fractions) {
      sum(values * coverage_fractions, na.rm = TRUE)
    }
  )
  
  # Check if pop_values is a matrix (multiple layers) and extract first layer
  if (is.matrix(pop_values) || is.data.frame(pop_values)) {
    pop_values <- as.numeric(pop_values[, 1])
  }
  
  # Add population to buildings
  buildings$population <- pop_values
  
  # Add building area while we're at it
  buildings$area <- as.numeric(st_area(buildings))
  
  # Filter to residential (population > 0)
  residential_buildings <- buildings %>%
    filter(population > 0)
  
  message("    Kept ", format(nrow(residential_buildings), big.mark = ","), 
          " buildings in populated areas (out of ", 
          format(nrow(buildings), big.mark = ","), ")")
  
  return(residential_buildings)
}

#' Load height categories from census block groups
#' @return SF object with height categories
load_height_categories <- function() {
  height_path <- here("Data", "building_heights")
  
  if (!dir.exists(height_path)) {
    dir.create(height_path, recursive = TRUE)
  }
  
  # Download if needed
  zip_file <- file.path(height_path, "Heights.zip")
  if (!file.exists(zip_file)) {
    url <- "https://www.sciencebase.gov/catalog/file/get/5775469ce4b07dd077c7088a?f=__disk__72%2F14%2Ff8%2F7214f853f18cbb9982d9b4046fa8e9f7bafd4664"
    download.file(url, zip_file, mode = "wb")
    unzip(zip_file, exdir = height_path)
  }
  
  # Read height data
  gdb_path <- list.files(here("Data", "building_heights", "srtm_derived_building_heights_by_block_group_conterminous_US"), pattern = "\\.gdb$", full.names = TRUE)[1]
  height_data <- st_read(gdb_path, quiet = TRUE) %>%
    dplyr::select(Height_cat, Shape)
  
  return(height_data)
}

#' Assign height categories to buildings
#' @param buildings Building footprints
#' @param height_data Height categories by block group
#' @return Buildings with heights
assign_building_heights <- function(buildings, height_data) {
  message("  Assigning height categories to buildings...")
  
  # Ensure same CRS
  height_data <- st_transform(height_data, st_crs(buildings))
  
  # Fix any invalid geometries
  height_data <- st_make_valid(height_data)
  
  # Spatial join
  buildings_with_height <- st_join(buildings, height_data, join = st_within)
  
  # Handle buildings without height assignment
  missing_height <- is.na(buildings_with_height$Height_cat)
  n_missing <- sum(missing_height)
  
  if (n_missing > 0) {
    message("  Imputing heights for ", format(n_missing, big.mark = ","), " buildings...")
    
    # Much faster approach: use spatial sampling or regional defaults
    if (n_missing < 10000) {
      # For smaller numbers, use nearest neighbor but vectorized
      missing_indices <- which(missing_height)
      missing_buildings <- buildings_with_height[missing_indices, ]
      
      # Use st_nearest_feature for vectorized nearest neighbor
      nearest_indices <- st_nearest_feature(missing_buildings, height_data)
      buildings_with_height$Height_cat[missing_indices] <- height_data$Height_cat[nearest_indices]
      
    } else {
      # For large numbers, use a faster heuristic
      message("    Using regional defaults for large-scale imputation...")
      
      # Option 1: Assign most common height category in the region
      # Get the most common height category
      height_table <- table(buildings_with_height$Height_cat[!missing_height])
      most_common_height <- names(height_table)[which.max(height_table)]
      
      # Assign to missing
      buildings_with_height$Height_cat[missing_height] <- most_common_height
      
      # Option 2 (alternative): Use Long Island vs rest of NY heuristic from original
      # Identify Long Island buildings
      long_island_bbox <- st_bbox(c(xmin = -74, ymin = 40.594, xmax = -71.856, ymax = 40.948), crs = 4326)
      long_island_polygon <- st_as_sfc(long_island_bbox) %>% st_transform(st_crs(buildings_with_height))
      
      in_long_island <- st_intersects(buildings_with_height[missing_height, ], long_island_polygon, sparse = FALSE)[, 1]
      
      # Assign based on location
      buildings_with_height$Height_cat[missing_height & in_long_island] <- "Low-medium"  # Common in Long Island
      buildings_with_height$Height_cat[missing_height & !in_long_island] <- "Low"  # Default for upstate
    }
  }
  
  # Ensure no NAs remain
  remaining_na <- sum(is.na(buildings_with_height$Height_cat))
  if (remaining_na > 0) {
    message("    Assigning 'Low' to ", remaining_na, " remaining buildings")
    buildings_with_height$Height_cat[is.na(buildings_with_height$Height_cat)] <- "Low"
  }
  
  return(buildings_with_height)
}

#' Impute missing ZCTA energy using dasymetric mapping
#' @param zcta_energy ZCTA energy data with missing values
#' @param community_energy Community energy data (complete)
#' @param buildings Buildings with heights and community assignments
#' @return ZCTA energy with imputed values
# impute_missing_zcta_energy <- function(zcta_energy, community_energy, buildings) {
#   message("Imputing missing ZCTA energy values...")
#   
#   # Identify months/years with missing data for each ZCTA
#   all_combinations <- expand.grid(
#     GEOID = unique(zcta_energy$GEOID),
#     year = unique(zcta_energy$year),
#     month = unique(zcta_energy$month)
#   )
#   
#   # Find missing combinations
#   existing_data <- zcta_energy %>%
#     filter(!is.na(True_Energy)) %>%
#     dplyr::select(GEOID, year, month) %>%
#     distinct()
#   
#   missing_combinations <- anti_join(all_combinations, existing_data, 
#                                     by = c("GEOID", "year", "month"))
#   
#   if (nrow(missing_combinations) == 0) {
#     message("  No missing ZCTA energy values to impute")
#     return(zcta_energy)
#   }
#   
#   message("  Found ", nrow(missing_combinations), " ZCTA-month combinations to impute")
#   
#   # Your existing imputation logic continues here...
#   # The key is to ensure the output maintains all the same columns as zcta_energy
#   # but with the is_imputed flag set appropriately
#   
#   # Apply dasymetric mapping (simplified version)
#   imputed_values <- apply_dasymetric_mapping_simple(
#     buildings, 
#     community_energy, 
#     missing_combinations
#   )
#   
#   # Merge imputed values back
#   zcta_energy_final <- zcta_energy %>%
#     left_join(
#       imputed_values %>% dplyr::select(GEOID, year, month, imputed_energy),
#       by = c("GEOID", "year", "month")
#     ) %>%
#     mutate(
#       total_energy = coalesce(total_energy, imputed_energy),
#       True_Energy = coalesce(True_Energy, imputed_energy),
#       is_imputed = is.na(True_Energy) & !is.na(imputed_energy)
#     )
#   
#   message("  Imputed ", sum(zcta_energy_final$is_imputed, na.rm = TRUE), " values")
#   
#   return(zcta_energy_final)
# }
impute_missing_zcta_energy <- function(zcta_energy, community_energy, buildings, hicdd_complete) {
  message("Imputing missing ZCTA energy values...")
  
  # Get all possible combinations
  all_combinations <- expand.grid(
    GEOID = unique(zcta_energy$GEOID),
    year = unique(zcta_energy$year),
    month = unique(zcta_energy$month),
    stringsAsFactors = FALSE
  )
  
  # Find existing combinations with actual energy data
  existing_data <- zcta_energy %>%
    filter(!is.na(True_Energy)) %>%
    dplyr::select(GEOID, year, month) %>%
    distinct()
  
  # Find missing combinations
  missing_combinations <- anti_join(all_combinations, existing_data, 
                                    by = c("GEOID", "year", "month"))
  
  if (nrow(missing_combinations) == 0) {
    message("  No missing ZCTA energy values to impute")
    return(zcta_energy)
  }
  
  message("  Found ", nrow(missing_combinations), " ZCTA-month combinations to impute")
  
  # Apply dasymetric mapping
  imputed_values <- apply_dasymetric_mapping( #apply_dasymetric_mapping_simple
    buildings, 
    community_energy, 
    zcta_energy #missing_combinations
  )
  
  # Get demographic template for each ZCTA-year
  template_data <- zcta_energy %>%
    group_by(GEOID, year) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    dplyr::select(-month, -True_Energy, -total_energy, -total_accounts, 
                  -Normalized_Energy, -groups, -groups_county, -groups_climate,
                  -HICDD)
  
  # Build complete imputed rows
  imputed_rows <- imputed_values %>%
    left_join(template_data, by = c("GEOID", "year")) %>%
    mutate(
      total_energy = imputed_energy,
      True_Energy = imputed_energy,
      total_accounts = NA_real_,
      is_imputed = TRUE,
      Normalized_Energy = imputed_energy / Households,
      groups = paste0(year, "_", month),
      groups_county = paste0(groups, "_", county_fips),
      groups_climate = paste0(groups, "_", Climate_Region)
    )
  
  # Add PROPER ZCTA-specific HICDD values
  hicdd_for_imputed <- hicdd_complete %>%
    rename(HICDD = mean) %>%
    dplyr::select(GEOID, year, month, HICDD)
  
  imputed_rows <- imputed_rows %>%
    left_join(hicdd_for_imputed, by = c("GEOID", "year", "month"))
  
  # Check for missing HICDD
  missing_hicdd <- sum(is.na(imputed_rows$HICDD))
  if (missing_hicdd > 0) {
    warning("  Warning: ", missing_hicdd, " imputed rows still missing HICDD values")
  }
  
  # Mark existing rows as not imputed
  zcta_energy_marked <- zcta_energy %>%
    mutate(is_imputed = FALSE)
  
  # COMBINE existing and imputed rows
  zcta_energy_final <- bind_rows(
    zcta_energy_marked,
    imputed_rows %>% dplyr::select(names(zcta_energy_marked))
  )
  
  message("  Original rows: ", nrow(zcta_energy))
  message("  Added imputed rows: ", nrow(imputed_rows))
  message("  Total rows: ", nrow(zcta_energy_final))
  
  return(zcta_energy_final)
}

apply_dasymetric_mapping <- function(buildings, community_energy, zcta_energy) {
  message("  Applying dasymetric mapping...")
  
  # First, assign buildings to geographies if needed
  if (!"GEOID_comm" %in% names(buildings) || !"GEOID_zcta" %in% names(buildings)) {
    message("    Assigning buildings to geographic units...")
    
    sf_use_s2(FALSE)
    
    # Load boundaries
    zctas <- get_zcta_boundaries()
    communities <- get_community_boundaries()
    
    # Transform to same CRS
    buildings_crs <- st_crs(buildings)
    zctas <- st_transform(zctas, buildings_crs)
    communities <- st_transform(communities, buildings_crs)
    
    # Fix geometries
    buildings <- st_make_valid(buildings)
    zctas <- st_make_valid(zctas)
    communities <- st_make_valid(communities)
    
    # Assign to communities and ZCTAs
    buildings <- st_join(buildings, communities %>% rename(GEOID_comm = GEOID), join = st_within)
    buildings <- st_join(buildings, zctas %>% rename(GEOID_zcta = GEOID), join = st_within)
    
    # Remove unmatched
    buildings <- buildings %>%
      filter(!is.na(GEOID_comm) & !is.na(GEOID_zcta))
    
    sf_use_s2(TRUE)
  }
  
  # Get optimized height scores
  height_scores <- optimize_height_scores(buildings, community_energy, zcta_energy)
  
  # Calculate building volumes WITH the height scores now defined
  buildings_with_volume <- buildings %>%
    mutate(
      height_score = height_scores[Height_cat],
      volume = effective_area * height_score  # effective_area already includes occupancy
    )
  
  # Aggregate volumes by community
  community_volumes <- buildings_with_volume %>%
    st_drop_geometry() %>%
    group_by(GEOID_comm) %>%
    summarize(total_volume = sum(volume, na.rm = TRUE), .groups = 'drop')
  
  # Process each time period
  all_periods <- expand.grid(
    year = unique(community_energy$year),
    month = unique(community_energy$month),
    stringsAsFactors = FALSE
  )
  
  imputed_list <- map_df(1:nrow(all_periods), function(i) {
    yr <- all_periods$year[i]
    mo <- all_periods$month[i]
    
    # Get community energy rates
    comm_energy_period <- community_energy %>%
      filter(year == yr, month == mo) %>%
      left_join(community_volumes, by = c("GEOID" = "GEOID_comm")) %>%
      mutate(energy_per_volume = if_else(total_volume > 0, True_Energy / total_volume, 0)) %>%
      dplyr::select(GEOID, energy_per_volume)
    
    # Apply to each building
    building_energy <- buildings_with_volume %>%
      left_join(comm_energy_period, by = c("GEOID_comm" = "GEOID")) %>%
      mutate(
        building_energy = volume * coalesce(energy_per_volume, 0)
      )
    
    # Aggregate to ZCTA
    zcta_aggregated <- building_energy %>%
      st_drop_geometry() %>%
      group_by(GEOID_zcta) %>%
      summarize(
        imputed_energy = sum(building_energy, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      rename(GEOID = GEOID_zcta) %>%
      mutate(year = yr, month = mo)
    
    return(zcta_aggregated)
  })
  
  return(imputed_list)
}

# apply_dasymetric_mapping_simple <- function(buildings, community_energy, missing_combinations) {
#   message("  Applying dasymetric mapping...")
#   
#   # Height scores from the original optimized values
#   height_scores <- c("Low" = 1.5, "Low-medium" = 2.5, "Medium" = 3.5, 
#                      "Medium-High" = 4.5, "High" = 6.5, "Very high" = 11)
#   
#   # Check if buildings are already assigned to geographies
#   if (!"GEOID_comm" %in% names(buildings) || !"GEOID_zcta" %in% names(buildings)) {
#     message("    Assigning buildings to geographic units...")
#     
#     # Turn off s2 for this operation
#     sf_use_s2(FALSE)
#     
#     # Load boundaries
#     zctas <- get_zcta_boundaries()
#     communities <- get_community_boundaries()
#     
#     # Ensure all geometries use the same CRS
#     buildings_crs <- st_crs(buildings)
#     zctas <- st_transform(zctas, buildings_crs)
#     communities <- st_transform(communities, buildings_crs)
#     
#     message("    CRS aligned: ", buildings_crs$input)
#     
#     # Fix any invalid geometries before joining
#     message("    Validating geometries...")
#     buildings <- st_make_valid(buildings)
#     zctas <- st_make_valid(zctas)
#     communities <- st_make_valid(communities)
#     
#     # Assign to communities first
#     buildings <- st_join(buildings, communities %>% rename(GEOID_comm = GEOID), 
#                          join = st_within)
#     
#     # Then assign to ZCTAs
#     buildings <- st_join(buildings, zctas %>% rename(GEOID_zcta = GEOID), 
#                          join = st_within)
#     
#     # Handle any unmatched buildings
#     unmatched_comm <- sum(is.na(buildings$GEOID_comm))
#     unmatched_zcta <- sum(is.na(buildings$GEOID_zcta))
#     
#     if (unmatched_comm > 0) {
#       message("    Warning: ", unmatched_comm, " buildings not matched to communities")
#     }
#     if (unmatched_zcta > 0) {
#       message("    Warning: ", unmatched_zcta, " buildings not matched to ZCTAs")
#     }
#     
#     # Remove unmatched buildings for now
#     buildings <- buildings %>%
#       filter(!is.na(GEOID_comm) & !is.na(GEOID_zcta))
#     
#     # Turn s2 back on
#     sf_use_s2(TRUE)
#   }
#   
#   # NOW the buildings should have GEOID_comm and GEOID_zcta columns
#   
#   # Calculate building volumes
#   buildings_with_volume <- buildings %>%
#     mutate(
#       height_score = height_scores[Height_cat],
#       volume = effective_area * height_score
#     )
#   
#   # Calculate RESIDENTIAL volume proportions
#   zcta_comm_proportions <- buildings_with_volume %>%
#     st_drop_geometry() %>%
#     group_by(GEOID_comm, GEOID_zcta) %>%
#     summarize(zcta_comm_volume = sum(volume, na.rm = TRUE), .groups = 'drop') %>%
#     group_by(GEOID_comm) %>%
#     mutate(
#       proportion_in_zcta = zcta_comm_volume / sum(zcta_comm_volume, na.rm = TRUE)
#     ) %>%
#     ungroup()
#   
#   # Process each time period
#   time_periods <- missing_combinations %>%
#     dplyr::select(year, month) %>%
#     distinct()
#   
#   imputed_results <- map_df(1:nrow(time_periods), function(i) {
#     yr <- time_periods$year[i]
#     mo <- time_periods$month[i]
#     
#     message("    Processing ", mo, "/", yr)
#     
#     # Get community RESIDENTIAL energy for this time period
#     community_energy_period <- community_energy %>%
#       filter(year == yr, month == mo) %>%
#       dplyr::select(GEOID, True_Energy)
#     
#     # Allocate community energy to ZCTAs based on residential volume proportions
#     zcta_allocated <- zcta_comm_proportions %>%
#       left_join(community_energy_period, by = c("GEOID_comm" = "GEOID")) %>%
#       mutate(
#         allocated_energy = True_Energy * proportion_in_zcta
#       ) %>%
#       group_by(GEOID_zcta) %>%
#       summarize(
#         imputed_energy = sum(allocated_energy, na.rm = TRUE),
#         .groups = 'drop'
#       ) %>%
#       mutate(
#         year = yr,
#         month = mo
#       ) %>%
#       rename(GEOID = GEOID_zcta)
#     
#     return(zcta_allocated)
#   })
#   
#   # Filter to only the missing combinations we need
#   imputed_values <- imputed_results %>%
#     inner_join(missing_combinations, by = c("GEOID", "year", "month"))
#   
#   message("    Generated ", nrow(imputed_values), " imputed values")
#   
#   return(imputed_values)
# }
#' Assign buildings to geographic units
#' @param buildings Buildings with heights
#' @return Buildings with geography assignments
assign_buildings_to_geographies <- function(buildings) {
  
  # Load boundaries
  zctas <- get_zcta_boundaries()
  communities <- get_community_boundaries()
  
  # Assign to communities
  buildings_comm <- st_join(buildings, communities, join = st_within)
  
  # Assign to ZCTAs
  buildings_final <- st_join(buildings_comm, zctas, join = st_within,
                             suffix = c("_comm", "_zcta"))
  
  return(buildings_final)
}


calculate_predicted_energy <- function(buildings, community_energy, scores) {
  # Ensure buildings have geography assignments
  if (!"GEOID_comm" %in% names(buildings) || !"GEOID_zcta" %in% names(buildings)) {
    sf_use_s2(FALSE)
    
    zctas <- get_zcta_boundaries()
    communities <- get_community_boundaries()
    
    buildings_crs <- st_crs(buildings)
    zctas <- st_transform(zctas, buildings_crs)
    communities <- st_transform(communities, buildings_crs)
    
    buildings <- st_make_valid(buildings)
    zctas <- st_make_valid(zctas)
    communities <- st_make_valid(communities)
    
    buildings <- st_join(buildings, communities %>% rename(GEOID_comm = GEOID), join = st_within)
    buildings <- st_join(buildings, zctas %>% rename(GEOID_zcta = GEOID), join = st_within)
    buildings <- buildings %>% filter(!is.na(GEOID_comm) & !is.na(GEOID_zcta))
    
    sf_use_s2(TRUE)
  }
  
  # Calculate volumes with given scores
  buildings_vol <- buildings %>%
    mutate(
      height_score = scores[Height_cat],
      volume = effective_area * height_score
    )
  
  # Aggregate volumes by community
  community_volumes <- buildings_vol %>%
    st_drop_geometry() %>%
    group_by(GEOID_comm) %>%
    summarize(total_volume = sum(volume, na.rm = TRUE), .groups = 'drop')
  
  # Calculate predicted energy for each ZCTA-month
  all_periods <- expand.grid(
    year = unique(community_energy$year),
    month = unique(community_energy$month),
    stringsAsFactors = FALSE
  )
  
  predicted_list <- map_df(1:nrow(all_periods), function(i) {
    yr <- all_periods$year[i]
    mo <- all_periods$month[i]
    
    # Get community energy rates
    comm_energy_period <- community_energy %>%
      filter(year == yr, month == mo) %>%
      left_join(community_volumes, by = c("GEOID" = "GEOID_comm")) %>%
      mutate(energy_per_volume = if_else(total_volume > 0, True_Energy / total_volume, 0)) %>%
      dplyr::select(GEOID, energy_per_volume)
    
    # Apply to buildings and aggregate to ZCTA
    building_energy <- buildings_vol %>%
      left_join(comm_energy_period, by = c("GEOID_comm" = "GEOID")) %>%
      mutate(building_energy = volume * coalesce(energy_per_volume, 0))
    
    zcta_predicted <- building_energy %>%
      st_drop_geometry() %>%
      group_by(GEOID_zcta) %>%
      summarize(predicted_energy = sum(building_energy, na.rm = TRUE), .groups = 'drop') %>%
      rename(GEOID = GEOID_zcta) %>%
      mutate(year = yr, month = mo)
    
    return(zcta_predicted)
  })
  
  return(predicted_list)
}
#' Optimize height scores using grid search
#' @param buildings Buildings with assignments
#' @param community_energy Community energy data
#' @param zcta_energy ZCTA energy data
#' @return Optimized height scores
optimize_height_scores <- function(buildings, community_energy, zcta_energy) {
  message("  Optimizing height scores...")
  
  # Get ZCTAs with known energy
  known_zctas <- zcta_energy %>%
    filter(!is.na(True_Energy)) %>%
    dplyr::select(GEOID, year, month, True_Energy)
  
  # Define optimization function
  optimize_fn <- function(scores) {
    names(scores) <- c("Low", "Low-medium", "Medium", "Medium-High", "High", "Very high")
    
    # Calculate predicted energy using these scores
    predicted <- calculate_predicted_energy(buildings, community_energy, scores)
    
    # Join with known values
    comparison <- known_zctas %>%
      inner_join(predicted, by = c("GEOID", "year", "month"))
    
    # Calculate RMSE
    rmse <- sqrt(mean((comparison$True_Energy - comparison$predicted_energy)^2, na.rm = TRUE))
    return(rmse)
  }
  
  # Optimize
  initial <- c(1.5, 2.5, 3.5, 4.5, 6.5, 11)
  result <- optim(
    par = initial,
    fn = optimize_fn,
    method = "L-BFGS-B",
    lower = rep(0.5, 6),
    upper = rep(20, 6)
  )
  
  optimized_scores <- result$par
  names(optimized_scores) <- c("Low", "Low-medium", "Medium", "Medium-High", "High", "Very high")
  
  message("  Optimized scores: ", paste(round(optimized_scores, 2), collapse = ", "))
  return(optimized_scores)
}


get_occupancy_ratios <- function() {
  message("  Getting occupancy ratios from census blocks...")
  
  # Get list of NY counties
  ny_counties <- counties(state = "36", year = 2010) %>%
    pull(COUNTYFP)
  
  # Download blocks by county and combine
  all_blocks <- map_df(ny_counties, function(county_code) {
    message("    Processing county ", county_code, "...")
    
    tryCatch({
      blocks <- get_decennial(
        geography = "block",
        variables = c(
          "Total_pop" = "P001001",
          "Total_status_units" = "H003001",
          "Vacant" = "H003003",
          "Occupied" = "H003002",
          "Total_units" = "H001001"
        ),
        year = 2010,
        output = "wide",
        geometry = TRUE,
        state = "36",
        county = county_code
      )
      return(blocks)
    }, error = function(e) {
      message("      Error with county ", county_code, ": ", e$message)
      return(NULL)
    })
  })
  
  # Process the combined data
  units_data <- all_blocks %>%
    st_transform(crs = 4326) %>%
    filter(!st_is_empty(.)) %>%
    filter(Total_pop >= 1) %>%
    mutate(
      occupied_ratio = Occupied / Total_units,
      occupied_ratio = if_else(is.nan(occupied_ratio), 1, occupied_ratio)
    ) %>%
    dplyr::select(occupied_ratio, geometry)
  
  message("    Loaded ", format(nrow(units_data), big.mark = ","), " census blocks")
  
  return(units_data)
}


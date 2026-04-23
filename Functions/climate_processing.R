# Climate Processing Functions
# Functions for calculating HICDD and assigning climate zones

#' Calculate Heat Index Cooling Degree Days
#' @param daymet_path Path to daymet data files
#' @param census_data SF object with geographic units
#' @param months Vector of months to process (5-9 for cooling season)
#' @return Dataframe with HICDD by geography, year, and month
# calculate_hicdd <- function(daymet_path, census_data, months = 5:9) {
#   message("Calculating Heat Index Cooling Degree Days...")
#   
#   # Get years from available files
#   tmax_files <- list.files(daymet_path, pattern = "tmax_daily_.*\\.nc$", full.names = TRUE)
#   years <- stringr::str_extract(basename(tmax_files), "\\d{4}") %>% as.numeric()
#   
#   # Process each year
#   hicdd_results <- map_df(years, function(year) {
#     message("  Processing year ", year, "...")
#     
#     # Process each month
#     month_results <- map_df(months, function(month) {
#       calculate_hicdd_month(daymet_path, year, month, census_data)
#     })
#     
#     return(month_results)
#   })
#   
#   message("  Calculated HICDD for ", length(unique(hicdd_results$GEOID)), 
#           " geographies across ", length(years), " years")
#   
#   return(hicdd_results)
# }
calculate_hicdd <- function(daymet_path, census_data, months = 5:9) {
  message("Calculating Heat Index Cooling Degree Days for ALL geographies...")
  
  # Get years from available files
  tmax_files <- list.files(daymet_path, pattern = "tmax_daily_.*\\.nc$", full.names = TRUE)
  years <- stringr::str_extract(basename(tmax_files), "\\d{4}") %>% as.numeric()
  
  # Process each year-month combination for ALL geographies at once
  hicdd_results <- map_df(years, function(year) {
    message("  Processing year ", year, "...")
    
    month_results <- map_df(months, function(month) {
      # Calculate for ALL census geographies at once
      calculate_hicdd_month(daymet_path, year, month, census_data)
    })
    
    return(month_results)
  })
  
  # Ensure we have ALL combinations (fill any missing with NA if needed)
  all_combinations <- expand.grid(
    GEOID = unique(census_data$GEOID),
    year = years,
    month = months,
    stringsAsFactors = FALSE
  )
  
  # Join to ensure completeness - KEEP the column name as 'mean' for compatibility
  hicdd_complete <- all_combinations %>%
    left_join(hicdd_results, by = c("GEOID", "year", "month"))
  
  # If any combinations are missing, they'll have NA for mean, which is fine
  
  message("  Calculated HICDD for ", nrow(hicdd_complete), " total combinations")
  message("  (", n_distinct(hicdd_complete$GEOID), " geographies x ", 
          length(years), " years x ", length(months), " months)")
  
  return(hicdd_complete)
}

#' Calculate HICDD for a specific month
#' @param daymet_path Path to daymet data
#' @param year Year to process
#' @param month Month to process
#' @param census_data Geographic units
#' @return HICDD for the specified month
calculate_hicdd_month <- function(daymet_path, year, month, census_data) {
  
  # Get date range for the month
  start_date <- as.Date(paste0(year, "-", sprintf("%02d", month), "-01"))
  end_date <- lubridate::ceiling_date(start_date, "month") - lubridate::days(1)
  days_in_month <- as.numeric(end_date - start_date) + 1
  
  # Calculate day indices in the year
  year_start <- as.Date(paste0(year, "-01-01"))
  start_index <- as.numeric(start_date - year_start) + 1
  end_index <- as.numeric(end_date - year_start) + 1
  
  # Open NetCDF files
  tmax_file <- file.path(daymet_path, paste0("tmax_daily_", year, "_ncss.nc"))
  vp_file <- file.path(daymet_path, paste0("vp_daily_", year, "_ncss.nc"))
  
  tmax_nc <- nc_open(tmax_file)
  vp_nc <- nc_open(vp_file)
  
  # Get coordinates
  lon <- ncvar_get(tmax_nc, "x")
  lat <- ncvar_get(tmax_nc, "y")
  
  # Extract data for the month
  tmax_month <- ncvar_get(tmax_nc, "tmax", 
                          start = c(1, 1, start_index),
                          count = c(-1, -1, days_in_month))
  
  vp_month <- ncvar_get(vp_nc, "vp",
                        start = c(1, 1, start_index),
                        count = c(-1, -1, days_in_month))
  
  # Close NetCDF files
  nc_close(tmax_nc)
  nc_close(vp_nc)
  
  # Convert temperature to Fahrenheit
  tmax_f <- tmax_month * 9/5 + 32
  
  # Calculate saturation vapor pressure
  sat_vp <- calculate_saturation_vp(tmax_month)
  
  # Calculate relative humidity
  rh <- (vp_month / sat_vp) * 100
  rh[rh > 100] <- 100
  rh[rh < 0] <- 0
  
  # Calculate heat index
  heat_index <- calculate_heat_index(tmax_f, rh)
  
  # Calculate cooling degree days (base 65F)
  cdd_daily <- pmax(heat_index - 65, 0)
  
  # Sum across days
  hicdd_month <- apply(cdd_daily, c(1, 2), sum, na.rm = TRUE)
  
  # Convert to raster in Daymet's native projection
  hicdd_raster <- raster(
    t(hicdd_month),
    xmn = min(lon), xmx = max(lon),
    ymn = min(lat), ymx = max(lat),
    crs = "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs"
  )
  
  # Transform census data to match Daymet projection instead of reprojecting raster
  census_data_daymet <- sf::st_transform(census_data, sf::st_crs(hicdd_raster))
  
  # Extract values for census geographies (no need for WGS84 raster)
  census_hicdd <- exact_extract(
    hicdd_raster,
    census_data_daymet,
    weights = "area",
    fun = "mean",
    append_cols = "GEOID"
  ) %>%
    mutate(
      year = year,
      month = month
    ) %>%
    dplyr::select(GEOID, year, month, mean)
  
  return(census_hicdd)
}

#' Calculate saturation vapor pressure
#' @param temp_c Temperature in Celsius
#' @return Saturation vapor pressure
calculate_saturation_vp <- function(temp_c) {
  ifelse(
    temp_c > 0,
    exp(34.494 - (4924.99 / (temp_c + 237.1))) / (temp_c + 105)^1.57,
    exp(43.494 - (6545.8 / (temp_c + 278))) / (temp_c + 868)^2
  )
}

#' Calculate heat index from temperature and humidity
#' @param temp_f Temperature in Fahrenheit
#' @param rh Relative humidity (%)
#' @return Heat index in Fahrenheit
calculate_heat_index <- function(temp_f, rh) {
  # Simplified heat index calculation
  # Use simple formula for lower temperatures
  hi <- temp_f
  
  # For temps above 80F, use full formula
  hot_idx <- which(temp_f > 80, arr.ind = TRUE)
  
  if (length(hot_idx) > 0) {
    t_hot <- temp_f[hot_idx]
    rh_hot <- rh[hot_idx]
    
    hi_hot <- -42.379 + 
      2.04901523 * t_hot + 
      10.14333127 * rh_hot - 
      0.22475541 * t_hot * rh_hot - 
      6.83783e-3 * t_hot^2 - 
      5.481717e-2 * rh_hot^2 + 
      1.22874e-3 * t_hot^2 * rh_hot + 
      8.5282e-4 * t_hot * rh_hot^2 - 
      1.99e-6 * t_hot^2 * rh_hot^2
    
    hi[hot_idx] <- hi_hot
  }
  
  return(hi)
}

#' Assign climate zones to counties
#' @param county_boundaries SF object with county boundaries
#' @return County boundaries with climate zone assignments
assign_climate_zones <- function(county_boundaries) {
  message("Assigning climate zones...")
  
  # Define climate zones using your correct mapping
  climate_zone4 <- tibble(
    clim_zone = "Mixed Humid",
    county_fips = c("36005", "36047", "36059", "36061", 
                    "36081", "36085", "36103", "36119")
  )
  
  climate_zone5 <- tibble(
    clim_zone = "Cool Humid",
    county_fips = c("36001", "36011", "36013", "36015", "36021", "36023",
                    "36027", "36029", "36037", "36039", "36051", "36055",
                    "36063", "36067", "36069", "36071", "36073", "36075",
                    "36079", "36083", "36087", "36091", "36093", "36099",
                    "36107", "36115", "36117", "36123")
  )
  
  climate_zone6 <- tibble(
    clim_zone = "Cold Humid",
    county_fips = c("36003", "36007", "36009", "36017", "36019", "36025",
                    "36031", "36033", "36035", "36041", "36043", "36045",
                    "36049", "36053", "36057", "36065", "36077", "36089",
                    "36095", "36097", "36101", "36105", "36109", "36111",
                    "36113", "36121")
  )
  
  # Combine all zones
  all_zones <- bind_rows(climate_zone4, climate_zone5, climate_zone6) %>%
    rename(Climate_Region = clim_zone)
  
  # Join with county boundaries
  counties_with_climate <- county_boundaries %>%
    left_join(all_zones, by = "county_fips")
  
  message("  Assigned ", n_distinct(counties_with_climate$Climate_Region), 
          " climate zones to ", nrow(counties_with_climate), " counties")
  
  return(counties_with_climate)
}

#' Match ZCTAs to counties and climate zones
#' @param zcta_boundaries ZCTA boundaries
#' @param county_boundaries County boundaries with climate zones
#' @param climate_zones Climate zone assignments
#' @return ZCTA to county/climate matching
match_zcta_to_county <- function(zcta_boundaries, county_boundaries, climate_zones) {
  message("Matching ZCTAs to counties...")
  
  # Use centroids for matching
  zcta_centroids <- st_centroid(zcta_boundaries)
  
  climate_zones <- st_transform(climate_zones, crs = "EPSG:2263")
  
  # Spatial join
  zcta_county <- st_join(zcta_centroids, climate_zones, join = st_within) %>%
    st_drop_geometry()
  
  # Handle any unmatched ZCTAs (e.g., on borders)
  unmatched <- zcta_county %>%
    filter(is.na(county_fips)) %>%
    pull(GEOID)
  
  if (length(unmatched) > 0) {
    message("  Handling ", length(unmatched), " unmatched ZCTAs...")
    
    # For unmatched, use nearest neighbor
    for (zcta_id in unmatched) {
      zcta_geom <- zcta_boundaries %>%
        filter(GEOID == zcta_id)
      
      distances <- st_distance(zcta_geom, climate_zones)
      nearest_idx <- which.min(distances)
      
      zcta_county[zcta_county$GEOID == zcta_id, c("county_fips", "NAME", "Climate_Region")] <-
        climate_zones[nearest_idx, c("county_fips", "NAME", "Climate_Region")] %>%
        st_drop_geometry()
    }
  }
  
  message("  Matched ", nrow(zcta_county), " ZCTAs to counties")
  
  return(zcta_county)
}

#' Match communities to counties and climate zones
#' @param community_demographics Community demographic data with geometry
#' @param county_boundaries County boundaries
#' @param climate_zones Climate zone assignments
#' @return Community to county/climate matching
match_communities_to_county <- function(community_demographics, county_boundaries, climate_zones) {
  message("Matching communities to counties...")
  
  # Use centroids for matching
  community_centroids <- st_centroid(community_demographics)
  
  # Spatial join
  community_county <- st_join(community_centroids, climate_zones, join = st_within) %>%
    st_drop_geometry() %>%
    dplyr::select(GEOID, county_fips, NAME, Climate_Region) %>%
    distinct()
  
  message("  Matched ", nrow(community_county), " communities to counties")
  
  return(community_county)
}

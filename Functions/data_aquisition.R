# Data Acquisition Functions
# Functions for downloading and loading raw data

#' Download ZCTA-level energy data from NY Open Data
#' @return dataframe with ZCTA energy consumption
download_zcta_energy_data <- function() {
  message("Downloading ZCTA energy data...")
  
  url <- "https://data.ny.gov/resource/tzb9-c2c6.geojson?$limit=1000000"
  shape <- st_read(url, quiet = TRUE)
  
  # Filter for residential electricity in cooling season
  filtered_shape <- shape %>%
    filter(
      data_field_display_name == "Residential Consumption (R)",
      data_class == "electricity",
      month %in% c(5, 6, 7, 8, 9),
      value > 0
    )
  
  # Load ZIP to ZCTA crosswalk
  crosswalk_path <- here("Data", "ZIPCodetoZCTACrosswalk2020UDS.xlsx")
  if (!file.exists(crosswalk_path)) {
    stop("ZIP to ZCTA crosswalk not found. Please place file at: ", crosswalk_path)
  }
  
  zip_crosswalk <- readxl::read_excel(crosswalk_path)
  
  # Join and aggregate to ZCTA level
  energy_per_zcta <- filtered_shape %>%
    st_drop_geometry() %>%
    left_join(zip_crosswalk, by = c("zip_code" = "ZIP_CODE")) %>%
    group_by(ZCTA, month, year) %>%
    summarise(
      total_energy = sum(as.numeric(value), na.rm = TRUE),
      total_accounts = sum(as.numeric(number_of_accounts), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(!is.na(ZCTA))
  
  message("  Downloaded ", nrow(energy_per_zcta), " ZCTA-month observations")
  
  return(energy_per_zcta)
}

#' Download community-level energy data from NY Open Data
#' @return sf object with community energy consumption
download_community_energy_data <- function() {
  message("Downloading community energy data...")
  
  # Download CSV data (faster than geojson for large datasets)
  c_url <- "https://data.ny.gov/resource/m3xm-q3dw.csv?$limit=2000000"
  community_data <- read_csv(c_url, show_col_types = FALSE) %>%
    mutate(FULL_FIPS = as.character(full_fips))
  
  # Filter for residential electricity in cooling season
  filtered_data <- community_data %>%
    filter(
      data_field_display_name == "Residential Consumption (R)",
      data_class == "electricity",
      month %in% c(5, 6, 7, 8, 9),
      value > 0
    )
  
  message("  Downloaded ", nrow(filtered_data), " community-month observations")
  
  return(filtered_data)
}

#' Get ZCTA boundaries from TIGER/Line
#' @param year Census year
#' @param state State name or FIPS code
#' @return sf object with ZCTA boundaries
get_zcta_boundaries <- function(year = 2010, state = "New York") {
  message("Loading ZCTA boundaries...")
  
  zctas_sf <- zctas(cb = FALSE, year = year, state = state) %>%
    st_transform(crs = "EPSG:2263") %>%
    rename(GEOID = ZCTA5CE10)
  
  message("  Loaded ", nrow(zctas_sf), " ZCTAs")
  
  return(zctas_sf)
}

#' Get community boundaries from GeoJSON file
#' @return sf object with community boundaries
get_community_boundaries <- function() {
  message("Loading community boundaries...")
  
  geojson_path <- here("Data", "communites_transformed.geojson")
  if (!file.exists(geojson_path)) {
    stop("Community boundaries file not found at: ", geojson_path)
  }
  
  communities <- st_read(geojson_path, quiet = TRUE) %>%
    st_transform(crs = "EPSG:2263") %>%
    rename(GEOID = FULL_FIPS)
  
  message("  Loaded ", nrow(communities), " communities")
  
  return(communities)
}

#' Get county boundaries from TIGER/Line
#' @param state State FIPS code
#' @param year Census year
#' @return sf object with county boundaries
get_county_boundaries <- function(state = "36", year = 2020) {
  message("Loading county boundaries...")
  
  counties_sf <- counties(state = state, year = year) %>%
    rename(county_fips = GEOID) %>%
    dplyr::select(county_fips, NAME, geometry)
  
  message("  Loaded ", nrow(counties_sf), " counties")
  
  return(counties_sf)
}

#' Get ACS demographic data for specified geography and years
#' @param geography Geographic level (zcta, place, county subdivision)
#' @param years Vector of years to retrieve
#' @return dataframe with demographic variables
get_acs_demographics <- function(geography, years) {
  message("Downloading ACS data for ", geography, "...")
  
  # Define variables to retrieve
  acs_vars <- c(
    "Median_Income_Household" = "B19013_001",
    "Total_pop" = "B01001_001",
    "White" = "B03002_003",
    "Black" = "B03002_004",
    "Asian" = "B03002_006",
    "Hispanic" = "B03001_003",
    "Households" = "B11001_001",
    "Avg_Household_Size" = "B25010_001",
    # Income distribution variables
    "Household_Income_All" = "B19001_001",
    # White non-Hispanic high-income bins (>=$100k)
    "Household_Income_White_high1" = "B19001H_014",
    "Household_Income_White_high2" = "B19001H_015",
    "Household_Income_White_high3" = "B19001H_016",
    "Household_Income_White_high4" = "B19001H_017",
    # Black low-income bins (<$25k)
    "Household_Income_Black_low1" = "B19001B_002",
    "Household_Income_Black_low2" = "B19001B_003",
    "Household_Income_Black_low3" = "B19001B_004",
    "Household_Income_Black_low4" = "B19001B_005",
    # Hispanic low-income bins (<$25k)
    "Household_Income_Hispanic_low1" = "B19001I_002",
    "Household_Income_Hispanic_low2" = "B19001I_003",
    "Household_Income_Hispanic_low3" = "B19001I_004",
    "Household_Income_Hispanic_low4" = "B19001I_005",
    # Asian low-income bins (<$25k)
    "Household_Income_Asian_low1" = "B19001D_002",
    "Household_Income_Asian_low2" = "B19001D_003",
    "Household_Income_Asian_low3" = "B19001D_004",
    "Household_Income_Asian_low4" = "B19001D_005"
  )
  
  # Retrieve data for each year
  acs_data <- map_df(years, function(yr) {
    message("  Year ", yr, "...")
    
    # Handle 2020 separately if needed (different survey availability)
    if (yr == 2020 && geography == "zcta") {
      # For 2020 ZCTA data, may need special handling
      data <- get_acs(
        geography = geography,
        variables = acs_vars,
        year = 2020,
        survey = "acs5",
        output = "wide",
        geometry = TRUE,
        #state = "36",
        show_call = FALSE
      ) 
    } else {
      data <- get_acs(
        geography = geography,
        variables = acs_vars,
        year = yr,
        survey = "acs5",
        output = "wide",
        geometry = TRUE,
        state = "36",
        show_call = FALSE
      )
    }

    # Process and add calculated fields
    data %>%
      # Remove the "E" suffix from estimate columns
      rename_with(~str_remove(., "E$"), ends_with("E")) %>%
      dplyr::select(!ends_with("M")) %>%
      mutate(
        year = yr,
        Other = Total_pop - White - Black - Asian - Hispanic,
        Size_per_House = Total_pop / Households,
        # Aggregate income categories
        # All_household_income_low = Household_Income_All_low1 + 
        #   Household_Income_All_low2 + 
        #   Household_Income_All_low3,
        # All_household_income_high = Household_Income_All_high1 + 
        #   Household_Income_All_high2 + 
        #   Household_Income_All_high3 + 
        #   Household_Income_All_high4,
        # White_household_income_high = Household_Income_White_high1 + 
        #   Household_Income_White_high2 + 
        #   Household_Income_White_high3 + 
        #   Household_Income_White_high4,
        # Black_household_income_low = Household_Income_Black_low1 + 
        #   Household_Income_Black_low2 + 
        #   Household_Income_Black_low3,
        # Hispanic_household_income_low = Household_Income_Hispanic_low1 + 
        #   Household_Income_Hispanic_low2 + 
        #   Household_Income_Hispanic_low3,
        # Asian_household_income_low = Household_Income_Asian_low1 + 
        #   Household_Income_Asian_low2 + 
        #   Household_Income_Asian_low3
      ) 
  })
  
  to_remove <- acs_data %>%
    st_drop_geometry() %>%
    group_by(GEOID) %>%
    summarize(n = n()) %>%
    filter(n==1)
  
  acs_data <- acs_data %>%
    filter(!GEOID %in% to_remove$GEOID)
  
  message("  Retrieved ", nrow(acs_data), " observations across ", 
          length(unique(acs_data$year)), " years")
  
  return(acs_data)
}

#' Download Daymet climate data
#' @param years Vector of years
#' @param bbox Bounding box coordinates (north, west, south, east)
#' @return List with temperature and vapor pressure data paths
download_daymet_data <- function(years, bbox) {
  message("Downloading Daymet climate data...")
  
  data_path <- here("Data", "daymet")
  if (!dir.exists(data_path)) {
    dir.create(data_path, recursive = TRUE)
  }
  
  # Check which years need downloading
  existing_files <- list.files(data_path, pattern = "\\.nc$")
  
  for (year in years) {
    tmax_file <- paste0("tmax_daily_", year, "_ncss.nc")
    vp_file <- paste0("vp_daily_", year, "_ncss.nc")
    
    if (!tmax_file %in% existing_files) {
      message("  Downloading temperature data for ", year, "...")
      download_daymet_ncss(
        location = bbox,
        start = year,
        end = year,
        param = "tmax",
        path = data_path
      )
    }
    
    if (!vp_file %in% existing_files) {
      message("  Downloading vapor pressure data for ", year, "...")
      download_daymet_ncss(
        location = bbox,
        start = year,
        end = year,
        param = "vp",
        path = data_path
      )
    }
  }
  
  message("  Climate data ready for years: ", paste(years, collapse = ", "))
  
  return(data_path)
}

#' Download Microsoft building footprints
#' @return sf object with building footprints
download_building_footprints <- function() {
  message("Downloading building footprints (this may take a while)...")
  
  buildings_path <- here("Data", "buildings")
  if (!dir.exists(buildings_path)) {
    dir.create(buildings_path, recursive = TRUE)
  }
  
  zip_file <- file.path(buildings_path, "NewYork.geojson.zip")
  geojson_file <- file.path(buildings_path, "NewYork.geojson")
  
  if (!file.exists(geojson_file)) {
    if (!file.exists(zip_file)) {
      url <- "https://minedbuildings.z5.web.core.windows.net/legacy/usbuildings-v2/NewYork.geojson.zip"
      message("  Downloading from: ", url)
      download.file(url, zip_file, mode = "wb")
    }
    
    message("  Extracting buildings...")
    unzip(zip_file, exdir = buildings_path)
  }
  
  message("  Reading building footprints...")
  buildings <- st_read(geojson_file, quiet = TRUE)
  
  message("  Loaded ", format(nrow(buildings), big.mark = ","), " buildings")
  
  return(buildings)
}

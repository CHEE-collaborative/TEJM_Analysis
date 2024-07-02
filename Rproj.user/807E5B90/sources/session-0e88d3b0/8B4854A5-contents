
Energy_download_zcta <- function(){
  
  # reading in the energy data
  url <- "https://data.ny.gov/resource/tzb9-c2c6.geojson?$limit=1000000"
  shape = st_read(url)
  
  # filtering the shapefile to contain only electricity data in the warm season months
  filtered_shape <- subset(shape, data_field_display_name == "Residential Consumption (R)" & data_class == "electricity" & month %in% c(5, 6, 7, 8, 9) & value > 0)
  
  # reading in zip code to ZCTA crosswalk
  zip_crosswalk <- read_excel(file.path(data_path, "ZIPCodetoZCTACrosswalk2020UDS.xlsx"))
  
  # Join the dataframes based on zip code
  filtered_shape <- as.data.frame(filtered_shape)
  energy_per_zcta <- filtered_shape %>%
    left_join(zip_crosswalk, by = c("zip_code" = "ZIP_CODE")) %>%
    dplyr::select(ZCTA, value, number_of_accounts, month, year) 

  # Calculate the sum of energy usage and number of accounts per ZCTA per month per year
  energy_per_zcta_summary <- energy_per_zcta %>%
    group_by(ZCTA, month, year) %>%
    summarise(total_energy = sum(as.numeric(value)),
              total_accounts = sum(as.numeric(number_of_accounts)))
  
  return(energy_per_zcta_summary)
}

Energy_download_community <- function(){
  
  # downloading community energy data 
  c_url = "https://data.ny.gov/resource/m3xm-q3dw.geojson?$limit=2000000"
  
  # getting zctas from tigris for outlines on plot
  zctas <- zctas(cb = FALSE, year = 2010, state = "New York")
  
  # setting the crs of the zctas
  zctas <- st_transform(zctas, crs = 4326)
  
  # reading community energy data from file
  c_shape = st_read(c_url)
  
  # selecting for only the data we want
  c_filtered_shape <- subset(c_shape, data_field_display_name == "Residential Consumption (R)" & data_class == "electricity" & month %in% c(5, 6, 7, 8, 9) & value > 0)
  
  # set the crs of the zctas shapefile to be the same as the energy
  c_zctas <- st_transform(zctas, crs = st_crs(c_filtered_shape))
  
  # join the zctas and the energy data
  c_joined <- st_join(c_zctas, c_filtered_shape)
  
  return(c_joined)
}

#Matching Each Building Polygon into a Community, a height category and a zcta area.
Building_Energy <-function(clean_buildings){
  
  # reading in community shapefiles provided by UER
  uer_communities <- st_read(file.path(data_path, "communites_transformed.geojson"))
  sf_use_s2(FALSE)
  clean_buildings_centroids <- clean_buildings
    
  # matching building polygons with the appropriate community (this takes around an hour to run)
  buildings_with_communities <- st_join(clean_buildings_centroids, uer_communities, join = st_within)
  
  # setting the crs of the zctas variable
  zctas <- zctas(cb = FALSE, year = 2010, state = "New York")
  zctas <- st_transform(zctas, crs = st_crs(buildings_with_communities))
  
  # matching building polygons with the appropriate zcta (this takes around 2.5 hours to run)
  buildings_with_communities_and_zctas <- st_join(buildings_with_communities, zctas, join = st_within)
  
  # Obtain Block areas and geometry data.
  if (!file.exists(file.path(data_path, "buildings", "Heights.zip"))) {
    url <- "https://www.sciencebase.gov/catalog/file/get/5775469ce4b07dd077c7088a?f=__disk__72%2F14%2Ff8%2F7214f853f18cbb9982d9b4046fa8e9f7bafd4664"
    destfile <- file.path(data_path, "buildings", "Heights.zip")
    download.file(url, destfile, mode = "wb")
  }
  # "https://www.sciencebase.gov/catalog/item/5775469ce4b07dd077c7088a#:~:text=srtm_derived_building_heights_by_block_group_conterminous_US.zip"
  if (!file.exists(file.path(data_path, "buildings", "srtm_bg_building_heights.gdb"))){
    destfile <- file.path(data_path, "buildings", "Heights.zip")
    unzip(destfile, exdir = file.path(data_path, "buildings"))
  }
  
  block_heights <- st_read(file.path(data_path,"buildings","srtm_derived_building_heights_by_block_group_conterminous_US","srtm_bg_building_heights.gdb")) %>%
    dplyr::select(Height_cat, Shape)
  block_heights <- st_transform(block_heights, st_crs(buildings_with_communities_and_zctas))
  
  # Fix invalid geometries
  # which(!st_is_valid(block_heights))
  block_heights <- st_make_valid(block_heights)
  
  # Crop The NY state
  # finding the extent of new york state 
  usa <- ne_states(country = "United States of America", returnclass = "sf")
  aoi <- usa[usa$name == "New York", ]
  aoi <- st_as_sf(aoi)
  ny_extent <- st_as_sfc(aoi) %>% st_geometry()
  ny_extent_larger <- st_buffer(ny_extent, dist = 0.1) %>% # To avoid borderline intersection
    st_transform(., st_crs(buildings_with_communities_and_zctas))
  
  block_heights <- st_crop(block_heights ,ny_extent_larger)
  
  # matching building polygons with the appropriate community (this takes around an hour to run)
  buildings_with_heights <- st_join(buildings_with_communities_and_zctas, block_heights, join = st_within)
  
  #We have 21 buildings without being assigned with a community and 30718 buildings without being assigned with a Height_cat.
  #Impute Communities
  missing_index_COM <- which(is.na(buildings_with_heights$COM_NAME)|is.na(buildings_with_heights$COM_TYPE))
  
  for(i in missing_index_COM) {
    distances <- st_distance(buildings_with_heights[i,], uer_communities)
    closest_point <- which.min(as.vector(distances))
    buildings_with_heights$COM_NAME[i] <- uer_communities$COM_NAME[closest_point]
    buildings_with_heights$COM_TYPE[i] <- uer_communities$COM_TYPE[closest_point]
  }
  
  
  # Impute heights
  missing_index_Height <- which(is.na(buildings_with_heights$Height_cat))
  missing_Height <- buildings_with_heights[missing_index_Height,]
  missing_Height$index <- missing_index_Height
  
  ## Impute missing values in Long Island
  ### bbox for long island
  long_island_bbox <- st_bbox(c(xmin = -74, ymin = 40.594, xmax = -71.856, ymax = 40.948), crs = 4326)
  long_island <- st_as_sfc(long_island_bbox) 
  long_island_missing_buildings <- st_intersection(missing_Height, long_island)
  missing_index_Height_Long_Island <- long_island_missing_buildings$index
  
  for(i in missing_index_Height_Long_Island) {
    distances <- st_distance(buildings_with_heights[i,], block_heights)
    closest_point <- which.min(as.vector(distances))
    buildings_with_heights$`Height_cat`[i] <- block_heights$Height_cat[closest_point]
  }
  
  ## Impute missing values in the middle part
  ### bbox for middle part
  missing_index_Height_Middle_part <- setdiff(missing_index_Height,missing_index_Height_Long_Island)
  missing_Height_middle_part <- buildings_with_heights[missing_index_Height_Middle_part,]
  middle_part_bbox <- st_bbox(missing_Height_middle_part, crs = 4326)
  middle_part <- st_as_sfc(middle_part_bbox) 
  middle_part_larger <- st_buffer(middle_part, dist = 0.1) #Get neighbor block groups
  block_heights_middle_part <- st_crop(block_heights ,middle_part_larger)
  
  #This area may have few buildings. We impute these building as "Low".
  buildings_with_heights$`Height_cat`[missing_index_Height_Middle_part] <- "Low"
  
  units_data <- get_decennial(
    geography = "block",
    variables = c("Total_pop" = "P001001",
                  "Total_status_units" = "H003001",
                  "Vacant" = "H003003",
                  "Occupied" = "H003002",
                  "Total_units" = "H001001"),
    year = 2010,
    output = "wide",
    geometry = TRUE,
    state = 36
  ) %>%
    st_transform(crs = st_crs(4326)) %>%
    filter(!st_is_empty(.)) %>% 
    filter(Total_pop >= 1) %>% 
    mutate(occupied_ratio = Occupied/Total_units) %>%
    mutate(occupied_ratio = ifelse(is.nan(occupied_ratio), 1, occupied_ratio)) %>%
    # as.data.frame(.) %>%
    dplyr::select(occupied_ratio, geometry)
  
  buildings_with_heights_ratio <- st_join(buildings_with_heights, units_data, join = st_within)
  
  return(buildings_with_heights_ratio)
}










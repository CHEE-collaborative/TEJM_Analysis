Dasy_Volume <- function(Height_score, buildings_data, validation,all_communities_energy_all_time){
  
  buildings_data <- buildings_data %>%
    st_drop_geometry(.) %>%
    mutate(Height = recode(Height_cat,
                           "Low" = Height_score[1],
                           "Low-medium" = Height_score[2],
                           "Medium" = Height_score[3],
                           "Medium-High" = Height_score[4],
                           "High" = Height_score[5],
                           "Very high" = Height_score[6])) %>%
    mutate(Volume = Height*effective_area)
  # mutate(Volume = Height*area)
  
  community_volume <- buildings_data %>%
    group_by(COM_NAME,COM_TYPE) %>%
    summarize(volume = sum(Volume))
  
  
  energy_all <- data.frame()
  
  for (selected_month in 5:9) {
    for (selected_year in 2016:2020) {
      
      
      cat("Processing Month:", selected_month, "Year:", selected_year, "\n")
      
      
      validation_month_year <- validation %>%
        filter(month == selected_month, year == selected_year)
      
      community_energy_data_month_year <- all_communities_energy_all_time %>%
        filter(month == selected_month, year == selected_year)
      
      # Add the area of the buildings for each community as a column in the dataset
      result <- left_join(community_energy_data_month_year, community_volume, by = c("COM_NAME","COM_TYPE"))
      
      ################################################################################
      
      # Calculate the energy used per square meter for each building in the county
      result$energy_per_cbm <- result$value / result$volume
      
      # Add energy per square meter to the buildings dataset for each building, according to which community each building is in
      
      test <- left_join(buildings_data, result, by = c("COM_NAME","COM_TYPE"))
      
      # Multiply energy_per_square_meter by the area of the buildings to get energy per building
      test$energy_per_building <- test$Volume * test$energy_per_cbm
      
      ################################################################################
      # Aggregate up to the ZCTA level, adding together energy per building
      
      energy_per_zcta_dasy <- test %>%
        mutate(GEOID = ZCTA5CE10) %>%
        group_by(GEOID) %>%
        summarize(total_energy_times_area = sum(energy_per_building, na.rm =F))
      
      validation_month_year <- validation_month_year %>%
        left_join(energy_per_zcta_dasy, by = c("GEOID"))
      
      
      energy_all <- bind_rows(energy_all,validation_month_year)
    }
  }
  
  
  MSE <- mean((as.numeric(energy_all$True_Energy)-as.numeric(energy_all$total_energy_times_area))^2,na.rm = T)
  
  result <- list(MSE = MSE, data = energy_all)
  return(result)
}

Adjusted_Dasymetric <- function(buildings_mapping, energy_zcta, energy_community){
  
  sf_use_s2(FALSE)
  #Load Buildings Data with Height
  buildings_data <- buildings_mapping %>%
    mutate(occupied_ratio = ifelse(is.na(occupied_ratio), 1, occupied_ratio)) %>%
    mutate(effective_area = area * occupied_ratio)
  
  #Load energy_community
  c_joined <- energy_community
  uer_communities = st_read(file.path(data_path, "communites_transformed.geojson"))
  zctas <- zctas(cb = FALSE, year = 2010, state = "New York")
  
  #Get validation dataset:
  validation <- data.frame()
  
  all_zctas <- zctas %>% 
    mutate(GEOID = ZCTA5CE10) %>% 
    st_drop_geometry(.) %>% 
    dplyr::select(GEOID) %>% 
    unique() %>% 
    filter(!is.na(GEOID))
  
  for(selected_month in 5:9){
    for(selected_year in 2016:2020){
      
      temp <- all_zctas%>% mutate(year = selected_year, month = selected_month)
      validation <- bind_rows(validation, temp)
      
    }
  }
  
  energy_zcta <- energy_zcta %>%
    # st_drop_geometry(.) %>%
    rename(GEOID = ZCTA) %>%
    mutate(year = as.integer(year),
           month = as.integer(month))
  
  validation <- validation %>%
    left_join(energy_zcta, by = c("GEOID", "year", "month")) %>%
    rename(True_Energy = total_energy) %>%
    mutate(True_Energy = ifelse(True_Energy == 0, NA, True_Energy))
  print(dim(validation))
  #Get Community Energy Data
  # Clean Community Energy Data
  community_energy_data = c_joined
  all_communities_all_time <- data.frame()
  
  all_communities <- buildings_data %>% 
    st_drop_geometry(.) %>% 
    dplyr::select(COM_NAME,COM_TYPE) %>%
    unique() %>%
    filter(!is.na(COM_NAME), !is.na(COM_TYPE))
  
  for(selected_month in 5:9){
    for(selected_year in 2016:2020){
      
      temp <- all_communities%>% mutate(year = selected_year, month = selected_month)
      all_communities_all_time <- bind_rows(all_communities_all_time, temp)
      
    }
  }
  
  community_energy_all_time <- community_energy_data %>%
    ungroup() %>%
    st_drop_geometry() %>%
    mutate(COM_NAME = com_name,
           COM_TYPE = com_type,
           COM_COUNTY = county_name,
           year = as.integer(year),
           month = as.integer(month)) %>%
    dplyr::select(COM_NAME, COM_TYPE, number_of_accounts, value, year, month) %>%
    filter(!is.na(value)) %>%
    arrange(desc(number_of_accounts)) %>%
    group_by(COM_NAME, COM_TYPE,year, month) %>%
    slice_head(n = 1)
  
  
  all_communities_energy_all_time <- all_communities_all_time %>%
    left_join(community_energy_all_time, by = c("COM_NAME", "COM_TYPE","year","month")) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(number_of_accounts = as.numeric(number_of_accounts))
  
  
  #A grid search to find best scores
  MSE <- array(dim = c(5,7,6))
  
  for(i in 1:5){
    for(j in 1:7){
      for(k in 1:6){
        time1 <- Sys.time()
        
        # score1 <- seq(1,2,by = 0.1)[i]
        # score2 <- seq(2,3,by = 0.1)[i]
        # score3 <- seq(3,4,by = 0.1)[i]
        score4 <- seq(4,6,length.out = 5)[i]
        score5 <- seq(6,9,length.out = 7)[j]
        score6 <- seq(10,15,length.out = 6)[k]
        
        Height_score <- c(1.5,2.5,3.5,4.5,6.5,11)
        # Height_score[1] <- score1
        # Height_score[2] <- score2
        # Height_score[3] <- score3
        Height_score[4] <- score4
        Height_score[5] <- score5
        Height_score[6] <- score6
        
        MSE[i,j,k] <- as.numeric(Dasy_Volume(Height_score, buildings_data, validation,all_communities_energy_all_time)[[1]]) 
        time2 <- Sys.time()
        print(time2-time1)
      }
      
    }
  }
  
  indices <- which(MSE == min(MSE),arr.ind = T)
  score4 <- seq(4,6,length.out = 5)[indices[1]]
  score5 <- seq(6,9,length.out = 7)[indices[2]]
  score6 <- seq(10,15,length.out = 6)[indices[3]]
  
  Height_score <- c(1.5,2.5,3.5,score4,score5,score6)
  result <- Dasy_Volume(Height_score, buildings_data, validation,all_communities_energy_all_time)
  print(paste0("The height score with minimum MSE is ",Height_score))
  
  return(result)
}








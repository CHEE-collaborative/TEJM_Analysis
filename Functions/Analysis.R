library(lqmm)
library(tidyverse)
library(sf)
library(tidycensus)
library(sjPlot)
library(here)
library(zoo)


ZCTA_County_Match <- function(){
  
  #api key for census tract data
  census_api_key("bdfb48e7e8ca85e6c72d18481ec5c0951b9c0abf", install = TRUE, overwrite = TRUE)
  
  GEOGRAPHIC_RESOLUTION = "zcta"

  get_acs_decennial <- function(geography,geometry = FALSE){
    acs_year <- get_decennial(
      geography = geography,
      variables = c("Total_pop" = "P001001"),
      year = 2010,
      output = "wide",
      geometry = geometry,
      state = 36
    )

    return(acs_year)
  }

  shape_files_zcta <- get_acs_decennial(GEOGRAPHIC_RESOLUTION, geometry = T) %>%
    select(GEOID, geometry) %>%
    mutate(GEOID = substr(GEOID, 3,7))

  zcta_centroids <- st_centroid(shape_files_zcta)

  shape_files_county <- get_acs_decennial("county", geometry = T) %>%
    mutate(county_fips = GEOID) %>%
    select(county_fips, NAME, geometry)
  # shape_files_county$NAME

  #R1: Western New York, Greate Lakes Plain
  #R2: Catskill Mountains and West Hudson River Valley
  #R3: Sounthern Tier
  #R4: New York City and Long Island
  #R5: East Hudson and Mohawk River Valleys
  #R6: Tug Hill Plateau
  #R7: Adirondack Mountains
  county_to_climate_region <- c("R5", "R3", "R4", "R3", "R1", "R1", "R3", "R2", "R7", "R7",
                                "R6", "R1", "R5", "R4", "R1", "R5", "R6", "R2", "R6", "R3",
                                "R4", "R5", "R4", "R2", "R7", "R5", "R5", "R1", "R1", "R1",
                                "R5", "R5", "R3", "R3", "R7", "R5", "R3", "R5", "R7", "R5",
                                "R1", "R2", "R5", "R4", "R6", "R1", "R5", "R4", "R4", "R2",
                                "R7", "R5", "R2", "R3", "R1", "R3", "R3", "R3", "R1", "R1",
                                "R1", "R2")

  shape_files_county$Climate_Region <- county_to_climate_region

  zcta_county_match <- st_join(zcta_centroids, shape_files_county, join = st_within) %>% st_drop_geometry()
  zcta_county_match <- zcta_county_match %>%
    mutate(county_fips = ifelse(GEOID == "13693" |GEOID == "13685", "36045", county_fips),
           NAME = ifelse(GEOID == "13693"|GEOID == "13685", "Jefferson County, New York", NAME),
           Climate_Region = ifelse(GEOID == "13693"|GEOID == "13685", "R6", Climate_Region)) %>%
    mutate(county_fips = ifelse(GEOID == "11957" |GEOID == "11939", "36103", county_fips),
           NAME = ifelse(GEOID == "11957" |GEOID == "11939", "Suffolk County, New York", NAME),
           Climate_Region = ifelse(GEOID == "11957" |GEOID == "11939", "R4", Climate_Region))
  
  return(zcta_county_match)
  
}


Model_income <- function(monthly_data){
  
  if(!dir.exists(file.path(result_path, "Model_Income"))){dir.create(file.path(result_path, "Model_Income"))}
  
  data_table <- monthly_data %>% 
    filter(!is.na(Total_Energy),!is.na(Avg_Household_Size),!is.na(Scaled_Income),!is.na(Households_scaled)) %>% 
    mutate(LSTCDD_Scaled_Income = LSTCDD*Scaled_Income)
  
  data_table_cat <- data_table %>%
    mutate(Median_Income_cat = ifelse(Imputed <= 50000,
                                      "<=50k",
                                      ifelse(Imputed <=70000,
                                             "50k-70k",
                                             ifelse(Imputed <= 90000,
                                                    "70k-90k",
                                                    ifelse(Imputed > 90000,
                                                           ">90k", NA))))) %>%
    mutate(Median_Income_cat = factor(Median_Income_cat,
                                      levels = c("<=50k","50k-70k","70k-90k",">90k")))
  
  zcta_county_match <- ZCTA_County_Match()
  data_table_cat <- data_table_cat %>%
    left_join(zcta_county_match, by = "GEOID") %>%
    mutate(groups_county = paste0(groups, "_", county_fips)) %>%
    mutate(groups_climate = paste0(groups, "_", Climate_Region))
  
  taus <- c(1,5,9)/10
  
  for(tau in taus){

    qm.effect_HICDD <- lqmm(fixed = Total_Energy ~ HICDD + 
                              Median_Income_cat + Avg_Household_Size + 
                              Households_scaled+
                              HICDD*Median_Income_cat,
                            random =  ~ 1,
                            group= factor(groups_county),
                            data = data_table_cat,
                            tau = tau,
                            control = lqmmControl(method = "df", UP_max_iter = 2000))
    
    summary_HICDD <- summary(qm.effect_HICDD)
    saveRDS(summary_HICDD, file.path(result_path, "Model_Income", paste0("model_summary_HICDD_categorical_income_",tau,".rds")))
  }
  
}


Model_ICE <- function(monthly_data){
  
  if(!dir.exists(file.path(result_path, "Model_ICE"))){dir.create(file.path(result_path, "Model_ICE"))}
  
  data_table_2 <- data_table %>%
    mutate(ICE_income = (All_household_income_high - All_household_income_low)/Households) %>%
    mutate(ICE_income_Black = (White_household_income_high - Black_household_income_low)/Households) %>%
    mutate(ICE_income_Hispanic = (White_household_income_high - Hispanic_household_income_low)/Households) %>%
    mutate(ICE_income_Asian = (White_household_income_high - Asian_household_income_low)/Households)
  zcta_county_match <- ZCTA_County_Match()
  data_table_2 <- data_table_2 %>%
    left_join(zcta_county_match, by = "GEOID") %>%
    mutate(groups_county = paste0(groups, "_", county_fips)) %>%
    mutate(groups_climate = paste0(groups, "_", Climate_Region))
  
  taus <- c(0.1, 0.5, 0.9)
  
  for(tau in taus){
    #ICE_Black:
    qm.effect_HICDD <- lqmm(fixed = Total_Energy ~ HICDD + 
                              ICE_income_Black + Avg_Household_Size + 
                              Households_scaled+
                              HICDD*ICE_income_Black,
                            random =  ~ 1,
                            group= factor(groups_county),
                            data = data_table_2,
                            tau = tau,
                            control = lqmmControl(method = "df", UP_max_iter = 2000))
    
    
    summary_HICDD <- summary(qm.effect_HICDD)
    saveRDS(summary_HICDD, file.path(result_path, "Model_ICE", paste0("model_summary_HICDD_ICE_income_Black_",tau,".rds")))

    #ICE_Hispanic:
    qm.effect_HICDD <- lqmm(fixed = Total_Energy ~ HICDD + 
                              ICE_income_Hispanic + Avg_Household_Size + 
                              Households_scaled+
                              HICDD*ICE_income_Hispanic,
                            random =  ~ 1,
                            group= factor(groups_county),
                            data = data_table_2,
                            tau = tau,
                            control = lqmmControl(method = "df", UP_max_iter = 2000))
    
    summary_HICDD <- summary(qm.effect_HICDD)
    saveRDS(summary_HICDD, file.path(result_path, "Model_ICE", paste0("model_summary_HICDD_ICE_income_Hispanic_",tau,".rds")))
    
    #ICE_Asian:
    print("Asian")
    qm.effect_HICDD <- lqmm(fixed = Total_Energy ~ HICDD + 
                              ICE_income_Asian + Avg_Household_Size + 
                              Households_scaled+
                              HICDD*ICE_income_Asian,
                            random =  ~ 1,
                            group= factor(groups_county),
                            data = data_table_2,
                            tau = tau,
                            control = lqmmControl(method = "df", UP_max_iter = 2000))
    
    summary_HICDD <- summary(qm.effect_HICDD)
    saveRDS(summary_HICDD, file.path(result_path, "Model_ICE", paste0("model_summary_HICDD_ICE_income_Asian_",tau,".rds")))
  }
}


Model_sensitivity <- function(monthly_data){
  
  if(!dir.exists(file.path(result_path, "Model_Sensitivity"))){dir.create(file.path(result_path, "Model_Sensitivity"))}
  
  data_table <- monthly_data %>% 
    filter(!is.na(Total_Energy),!is.na(Avg_Household_Size),!is.na(Scaled_Income),!is.na(Households_scaled)) %>% 
    mutate(LSTCDD_Scaled_Income = LSTCDD*Scaled_Income)
  
  data_table_cat <- data_table %>%
    mutate(Median_Income_cat = ifelse(Imputed <= 50000,
                                      "<=50k",
                                      ifelse(Imputed <=60000,
                                             "50k-60k",
                                             ifelse(Imputed <= 70000,
                                                    "60k-70k",
                                                    ifelse(Imputed <= 80000,
                                                           "70k-80k", ifelse(Imputed <= 90000,
                                                                             "80k-90k", ifelse(Imputed <= 100000,
                                                                                               "90k-100k", ifelse(Imputed > 100000,
                                                                                                                  ">100k", NA)))))))) %>%
    mutate(Median_Income_cat = factor(Median_Income_cat,
                                      levels = c("<=50k","50k-60k","60k-70k", "70k-80k","80k-90k", "90k-100k", ">100k")))
  
  zcta_county_match <- ZCTA_County_Match()
  data_table_cat <- data_table_cat %>%
    left_join(zcta_county_match, by = "GEOID") %>%
    mutate(groups_county = paste0(groups, "_", county_fips)) %>%
    mutate(groups_climate = paste0(groups, "_", Climate_Region))
  
  taus <- c(0.1,0.2,0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  
  for(tau in taus){
    
    qm.effect_HICDD <- lqmm(fixed = Total_Energy ~ HICDD + 
                              Median_Income_cat + Avg_Household_Size + 
                              Households_scaled+
                              HICDD*Median_Income_cat,
                            random =  ~ 1,
                            group= factor(groups_county),
                            data = data_table_cat,
                            tau = tau,
                            control = lqmmControl(method = "df", UP_max_iter = 2000))
    
    summary_HICDD <- summary(qm.effect_HICDD)
    saveRDS(summary_HICDD, file.path(result_path, "Model_Sensitivity", paste0("model_summary_HICDD_categorical_income_more_",tau,".rds")))
  }
  
}





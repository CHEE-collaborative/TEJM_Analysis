
#Create data for income categories and ICE

get_acs_decennial <- function(geography,geometry = FALSE){
  acs_year <- get_decennial(
    geography = geography,
    variables = c("Total_pop" = "P001001",
                  "White" = "P005003",
                  "Black" = "P005004",
                  "Asian" = "P005006",
                  "Hispanic" = "P005010",
                  "Households" = "H003001"),
    year = 2010,
    output = "wide",
    geometry = geometry,
    state = 36
  ) %>%
    #st_transform(crs = st_crs(4326)) %>%
    # filter(!st_is_empty(.)) %>%
    mutate(Other = Total_pop - White - Black - Asian - Hispanic,
           Size_per_House = Total_pop/Households)
  return(acs_year)
}

get_acs_5year <- function(geography,year){
  acs_year <- get_acs(
    geography = geography,
    variables = c("Median_Income_Household" = "B19013_001E",
                  "Total_pop" = "B01001_001E",
                  "White" = "B03002_003E",
                  "Black" = "B03002_004E",
                  "Asian" = "B03002_006E",
                  "Hispanic" = "B03001_003E",
                  "Households" = "B11001_001E",
                  "Avg_Household_Size" = "B25010_001E",
                  "Household_Income_All" = "B19001_001E",
                  "Household_Income_All_low1" = "B19001_002E",
                  "Household_Income_All_low2" = "B19001_003E",
                  "Household_Income_All_low3" = "B19001_004E",
                  "Household_Income_All_high1" = "B19001_014E",
                  "Household_Income_All_high2" = "B19001_015E",
                  "Household_Income_All_high3" = "B19001_016E",
                  "Household_Income_All_high4" = "B19001_017E",
                  "Household_Income_White" = "B19001H_001E",
                  "Household_Income_White_high1" = "B19001H_014E",
                  "Household_Income_White_high2" = "B19001H_015E",
                  "Household_Income_White_high3" = "B19001H_016E",
                  "Household_Income_White_high4" = "B19001H_017E",
                  "Household_Income_Black" = "B19001B_001E",
                  "Household_Income_Black_low1" = "B19001B_002E",
                  "Household_Income_Black_low2" = "B19001B_003E",
                  "Household_Income_Black_low3" = "B19001B_004E",
                  "Household_Income_Hispanic" = "B19001I_001E",
                  "Household_Income_Hispanic_low1" = "B19001I_002E",
                  "Household_Income_Hispanic_low2" = "B19001I_003E",
                  "Household_Income_Hispanic_low3" = "B19001I_004E",
                  "Household_Income_Asian" = "B19001D_001E",
                  "Household_Income_Asian_low1" = "B19001D_002E",
                  "Household_Income_Asian_low2" = "B19001D_003E",
                  "Household_Income_Asian_low3" = "B19001D_004E"
    ),
    year = year,
    survey = "acs5",
    output = "wide",
    geometry = FALSE,
    state = 36
  ) %>%
    mutate(Other = Total_pop - White - Black - Asian - Hispanic,
           Size_per_House = Total_pop/Households,
           All_household_income_low = Household_Income_All_low1+Household_Income_All_low2+Household_Income_All_low3,
           All_household_income_high = Household_Income_All_high1+Household_Income_All_high2+Household_Income_All_high3+Household_Income_All_high4,
           White_household_income_high = Household_Income_White_high1+Household_Income_White_high2+Household_Income_White_high3+Household_Income_White_high4,
           Black_household_income_low = Household_Income_Black_low1+Household_Income_Black_low2+Household_Income_Black_low3,
           Hispanic_household_income_low = Household_Income_Hispanic_low1+Household_Income_Hispanic_low2+Household_Income_Hispanic_low3,
           Asian_household_income_low = Household_Income_Asian_low1 + Household_Income_Asian_low2 + Household_Income_Asian_low3)
  return(acs_year)
}


Merge_Demographics <- function(Processed_Energy, HICDD){
  
  GEOGRAPHIC_RESOLUTION <- "zcta"
  
  monthly_data <- Processed_Energy %>%
    arrange(year, month) %>%
    group_by(GEOID) %>%
    mutate(total_accounts = ifelse(total_accounts == 0, NA, total_accounts),
           True_Energy = ifelse(True_Energy == 0, NA, True_Energy)) %>%
    fill(total_accounts, .direction = "downup") %>%
    fill(total_accounts, .direction = "updown") %>%
    mutate(Total_Energy = ifelse(is.na(True_Energy), total_energy_times_area, True_Energy))
  
  median_income_yearly <- NULL
  for(year_num in 2016:2019){
    acs_data_yearly <- get_acs_5year(GEOGRAPHIC_RESOLUTION,year_num) %>%
      mutate(year = year_num) %>%
      dplyr::select(GEOID,Median_Income_Household, 
                    Total_pop,White, Black, Asian, Hispanic, Other,
                    Households,Size_per_House,Avg_Household_Size,
                    year,
                    Household_Income_All,
                    All_household_income_low,All_household_income_high,
                    White_household_income_high, Black_household_income_low,
                    Hispanic_household_income_low, Asian_household_income_low)
    median_income_yearly <- rbind(median_income_yearly,acs_data_yearly)
  }
  acs_data_2020 <- get_acs(
    geography = GEOGRAPHIC_RESOLUTION,
    variables = c("Median_Income_Household" = "B19013_001E",
                  "Total_pop" = "B01001_001E",
                  "White" = "B03002_003E",
                  "Black" = "B03002_004E",
                  "Asian" = "B03002_006E",
                  "Hispanic" = "B03001_003E",
                  "Households" = "B11001_001E",
                  "Avg_Household_Size" = "B25010_001E",
                  "Household_Income_All" = "B19001_001E",
                  "Household_Income_All_low1" = "B19001_002E",
                  "Household_Income_All_low2" = "B19001_003E",
                  "Household_Income_All_low3" = "B19001_004E",
                  "Household_Income_All_high1" = "B19001_014E",
                  "Household_Income_All_high2" = "B19001_015E",
                  "Household_Income_All_high3" = "B19001_016E",
                  "Household_Income_All_high4" = "B19001_017E",
                  "Household_Income_White" = "B19001H_001E",
                  "Household_Income_White_high1" = "B19001H_014E",
                  "Household_Income_White_high2" = "B19001H_015E",
                  "Household_Income_White_high3" = "B19001H_016E",
                  "Household_Income_White_high4" = "B19001H_017E",
                  "Household_Income_Black" = "B19001B_001E",
                  "Household_Income_Black_low1" = "B19001B_002E",
                  "Household_Income_Black_low2" = "B19001B_003E",
                  "Household_Income_Black_low3" = "B19001B_004E",
                  "Household_Income_Hispanic" = "B19001I_001E",
                  "Household_Income_Hispanic_low1" = "B19001I_002E",
                  "Household_Income_Hispanic_low2" = "B19001I_003E",
                  "Household_Income_Hispanic_low3" = "B19001I_004E",
                  "Household_Income_Asian" = "B19001D_001E",
                  "Household_Income_Asian_low1" = "B19001D_002E",
                  "Household_Income_Asian_low2" = "B19001D_003E",
                  "Household_Income_Asian_low3" = "B19001D_004E"),
    year = 2020,
    survey = "acs5",
    output = "wide",
    geometry = FALSE) %>%
    filter(GEOID %in% median_income_yearly$GEOID) %>%
    mutate(year = 2020) %>%
    mutate(Other = Total_pop - White - Black - Asian - Hispanic,
           Size_per_House = Total_pop/Households,
           All_household_income_low = Household_Income_All_low1+Household_Income_All_low2+Household_Income_All_low3,
           All_household_income_high = Household_Income_All_high1+Household_Income_All_high2+Household_Income_All_high3+Household_Income_All_high4,
           White_household_income_high = Household_Income_White_high1+Household_Income_White_high2+Household_Income_White_high3+Household_Income_White_high4,
           Black_household_income_low = Household_Income_Black_low1+Household_Income_Black_low2+Household_Income_Black_low3,
           Hispanic_household_income_low = Household_Income_Hispanic_low1+Household_Income_Hispanic_low2+Household_Income_Hispanic_low3,
           Asian_household_income_low = Household_Income_Asian_low1 + Household_Income_Asian_low2 + Household_Income_Asian_low3) %>%
    dplyr::select(GEOID,Median_Income_Household, 
                  Total_pop,White, Black, Asian, Hispanic, Other,
                  Households,Size_per_House,Avg_Household_Size,
                  year,
                  Household_Income_All,
                  All_household_income_low,All_household_income_high,
                  White_household_income_high, Black_household_income_low,
                  Hispanic_household_income_low, Asian_household_income_low)
  
  
  median_income_yearly <- rbind(median_income_yearly,acs_data_2020)
  
  median_income_yearly <- median_income_yearly %>%
    group_by(GEOID) %>%
    arrange(year) %>%
    mutate(Imputed = 
             ifelse(rep(all(is.na(Median_Income_Household)),length(year)), NA,
                    na.approx(Median_Income_Household, year,rule = 2))) %>%
    ungroup()
  
  HICDD_monthly <- HICDD %>%
    mutate(GEOID = substr(GEOID,3,7),
           HICDD = mean) %>%
    dplyr::select(GEOID, year, month, HICDD)
  
  monthly_data <- monthly_data %>%
    left_join(HICDD_monthly, by = c("GEOID","year","month")) %>%
    left_join(median_income_yearly, by = c("GEOID", "year")) %>%
    mutate(White_prop = White/Total_pop,
           Black_prop = Black/Total_pop,
           Asian_prop = Asian/Total_pop,
           Hispanic_prop = Hispanic/Total_pop) 
  
  
  monthly_data <- monthly_data %>%
    mutate(groups = paste0(year,"_",month))
  
  data_table <- monthly_data %>% 
    filter(!is.na(Total_Energy),!is.na(Avg_Household_Size),!is.na(Imputed),!is.na(Households))
  
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
  
  data_table_2 <- data_table_cat %>%
    mutate(ICE_income = (All_household_income_high - All_household_income_low)/Households) %>%
    mutate(ICE_income_Black = (White_household_income_high - Black_household_income_low)/Households) %>%
    mutate(ICE_income_Hispanic = (White_household_income_high - Hispanic_household_income_low)/Households) %>%
    mutate(ICE_income_Asian = (White_household_income_high - Asian_household_income_low)/Households)
  
  data_table_final <- data_table_2 %>%
    arrange(year, month) %>%
    group_by(GEOID) %>%
    mutate(total_accounts = ifelse(total_accounts == 0, NA, total_accounts),
           True_Energy = ifelse(True_Energy == 0, NA, True_Energy)) %>%
    fill(total_accounts, .direction = "downup") %>%
    fill(total_accounts, .direction = "updown") %>%
    mutate(Total_Energy = ifelse(is.na(True_Energy), total_energy_times_area, True_Energy))
  
  zcta_county_match <- readRDS(file.path(data_path, "zcta_county_climate_match.rds"))
  
  data_table_final <- data_table_2 %>%
    left_join(zcta_county_match, by = "GEOID") %>%
    mutate(groups_county = paste0(groups, "_", county_fips)) %>%
    mutate(groups_climate = paste0(groups, "_", Climate_Region)) %>%
    mutate(Normalized_Energy = ifelse(is.na(total_accounts),
                                      Total_Energy/Households,
                                      Total_Energy/total_accounts
    )) %>%
    filter(!is.na(Normalized_Energy))
  
  return(data_table_final)
  
}

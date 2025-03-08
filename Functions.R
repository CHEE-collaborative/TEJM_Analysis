source(file.path(here("Functions"),"HICDD_process.R"))
source(file.path(here("Functions"),"Energy_process.R"))
source(file.path(here("Functions"),"Buildings_process.R"))
source(file.path(here("Functions"),"Adjusted_Dasymetric.R"))
source(file.path(here("Functions"),"Demographics_data_process.R"))
source(file.path(here("Functions"),"Models.R"))

Generate_HICDD <- function(){
  
  data_path <- here("Data")
  NY_BBOX <- c(46,-80,40,-71)
  census_data <- GetCensusData("zcta")
  DownloadDaymetData(2016:2020, NY_BBOX)
  HICDD <- calculateHICDD(2016:2020, census_data =  census_data)
  saveRDS(HICDD, file.path(data_path, "HICDD.rds"))
  return(HICDD)
  
}

Process_Buildings <- function(){
  merged_raster <- Merge_raster()
  buildings <- Building_filter(merged_raster)
  
  return(buildings)
}

Building_merge <- function(buildings){
  buildings_with_heights <- Building_Energy(buildings)
  return(buildings_with_heights)
}


Process_Energy <- function(buildings_processed, energy_zcta, energy_community){
  
  # data_path <- here("Data")
  # energy_zcta <- Energy_download_zcta()
  # saveRDS(energy_zcta, file.path(data_path, "energy", "energy_zcta.rds"))
  # energy_community <-Energy_download_community()
  # saveRDS(energy_community, file.path(data_path, "energy", "energy_community.rds"))

  # buildings_with_heights <- Building_Energy(buildings)
  result <- Adjusted_Dasymetric(buildings_processed, energy_zcta, energy_community)
  
  print(paste0("The MSE of Dasymetric Imputation is ", result[[1]],"."))
  data <- result$data
  
  return(data)
}

Demographics_Merge <-function(Processed_Energy, HICDD){
  
  census_api_key("bdfb48e7e8ca85e6c72d18481ec5c0951b9c0abf", install = TRUE, overwrite = TRUE)
  
  Final_Data <- Merge_Demographics(Processed_Energy, HICDD)
  
  return(Final_Data)
  
}

Main_Analysis <- function(Merged_Data, Energy_community){
  
  if(!dir.exists(file.path(here(), "Outputs"))){dir.create(file.path(here(), "Outputs"))}
  
  # Energy Overview
  Figure_1(Merged_Data)
  Figure_2(Merged_Data)  # Add this line to call the new function
  # Main Analysis
  Main_Model(Merged_Data)
  
}
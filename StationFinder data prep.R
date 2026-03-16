# Preparing data for StationFinder

library(tidyverse)
library(sf)

# STATION-YEAR DATA 
station21 <- readRDS("processed/stations/by year/stations_byyear_2021.rds")
station22 <- readRDS("processed/stations/by year/stations_byyear_2022.rds")
station23 <- readRDS("processed/stations/by year/stations_byyear_2023.rds")
station24 <- readRDS("processed/stations/by year/stations_byyear_2024.rds")
station25 <- readRDS("processed/stations/by year/stations_byyear_2025.rds")

station <- rbind(station21,
                 station22,
                 station23,
                 station24,
                 station25)

station_0docks <- station %>% filter(total_docks == 0)
saveRDS(station_0docks, "processed/stations/by year/redactedstations.rds")

# final station-year dataset for StationFinder
station_drop0docks <- station %>% filter(total_docks != 0) %>% 
  mutate(TOWN = toupper(municipality))
saveRDS(station_drop0docks, "processed/stations/by year/stations_byyear.rds") 

################################################################################

bike_TOWN = toupper(unique(station$municipality))

# MAP DATA
MA_sf <- st_read("data/townssurvey_shp/TOWNSSURVEY_POLY.shp") %>% 
  select(TOWN, COUNTY, SHAPE_Leng, SHAPE_Area, geometry) %>%
  filter(TOWN %in% bike_TOWN) # only include towns with bluebike docks (this may or not be accurate, need to double check)

MA_grid <- MA_sf %>% st_make_grid(n=c(100,100)) # make grid version of MA_sf

# make grid map that isolates the grid pattern within the town regions
MA_grid_map <-st_intersection(MA_sf, 
                              MA_grid) %>% 
  st_make_valid() %>% 
  mutate(grid_id = row_number()) %>% 
  select(grid_id, TOWN, COUNTY, geometry)

write_sf(MA_grid_map, "processed/region_grid_map.shp")

################################################################################

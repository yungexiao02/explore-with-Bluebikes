# combine datasets by year (2021-2025)
## note that data collection changed in 2023 -- adjust all prior to match April 2023 and forward 

library(tidyverse)
library(data.table)

################################################################################
########################## 2021-2022, 2024-2025 ################################
################################################################################

start_path = "data/ride data extracts/"
year = c("2021", "2022", "2024", "2025") # have to do 2023 separately because it changed between 202303-202304
month = sprintf("%02d", 1:12)
end_path = "-bluebikes-tripdata.csv"

# 2021-2022, 2024-2025
for (yr in year) {
  message("Processing year: ", yr)
  
  month_data <- list()
  
  for (mo in month) {
    file_url <- paste0(start_path, yr, mo, end_path)
    message("  Downloading month: ", mo)
    
    # fread can read directly from zip URL if it's a CSV inside
    # Otherwise, you can download+unzip first, then fread
    month_data[[mo]] <- fread(file_url)
  }
  
  # Combine all months for the year
  year_df <- rbindlist(month_data)
  
  # Save to disk
  fwrite(year_df, file = paste0("processed/rides_", yr, ".csv"))
  
  # Free memory
  rm(month_data, year_df)
  gc()
}

################################################################################
################################### 2023 #######################################
################################################################################

month_1 = month[1:3] # this is when data collection changed
month_2 = month[4:12]

month_data_1 = list() 
month_data_2 = list() 

for (mo in month_1) {
  file_url <- paste0(start_path, "2023", mo, end_path)
  message("  Downloading month: ", mo)
  
  # fread can read directly from zip URL if it's a CSV inside
  # Otherwise, you can download+unzip first, then fread
  month_data_1[[mo]] <- fread(file_url)
}

for (mo in month_2) {
  file_url <- paste0(start_path, "2023", mo, end_path)
  message("  Downloading month: ", mo)
  
  # fread can read directly from zip URL if it's a CSV inside
  # Otherwise, you can download+unzip first, then fread
  month_data_2[[mo]] <- fread(file_url)
}


# Combine all months for 2023, 2 parts 
rides_2023_df_1 <- rbindlist(month_data_1)
rides_2023_df_2 <- rbindlist(month_data_2)

## Fix column names to match
rides_2023_df_1 <- rides_2023_df_1 %>% 
  transmute(ride_id = NA,
         rideable_type = NA, 
         started_at = starttime,
         ended_at = stoptime,
         start_station_name = `start station name`,
         start_station_id = `start station id`,
         end_station_name = `end station name`,
         end_station_id = `end station id`,
         start_lat = `start station latitude`,
         start_lng = `start station longitude`,
         end_lat = `end station latitude`,
         end_lng = `end station longitude`,
         member_casual = NA) 

rides_2023 <- rbind(rides_2023_df_1, rides_2023_df_2)

# Save to disk
fwrite(rides_2023, file = paste0("processed/rides_", "2023", ".csv"))

# Free memory
rm(month_data_1, month_data_2, rides_2023)
gc()

################################################################################
######################### FIX 2021-2022 to match ###############################
################################################################################

rides_2021 <- read.csv("processed/rides_2021.csv")

rides_2021_adj <- rides_2021 %>% 
  transmute(ride_id = bikeid,
            rideable_type = NA, 
            started_at = starttime,
            ended_at = stoptime,
            start_station_name = start.station.name,
            start_station_id = start.station.id,
            end_station_name = end.station.name,
            end_station_id = end.station.id,
            start_lat = start.station.latitude,
            start_lng = start.station.longitude,
            end_lat = end.station.latitude,
            end_lng = end.station.latitude,
            member_casual = ifelse(usertype == "Customer", "casual",
                                   ifelse(usertype == "Subscriber", "member", NA))) 

fwrite(rides_2021_adj, file = paste0("processed/rides_", "2021", "_adj.csv"))
rm(rides_2021, rides_2021_adj)

rides_2022 <- read.csv("processed/rides_2022.csv")

rides_2022_adj <- rides_2022 %>% 
  transmute(ride_id = bikeid,
            rideable_type = NA, 
            started_at = starttime,
            ended_at = stoptime,
            start_station_name = start.station.name,
            start_station_id = start.station.id,
            end_station_name = end.station.name,
            end_station_id = end.station.id,
            start_lat = start.station.latitude,
            start_lng = start.station.longitude,
            end_lat = end.station.latitude,
            end_lng = end.station.latitude,
            member_casual = ifelse(usertype == "Customer", "casual",
                                   ifelse(usertype == "Subscriber", "member", NA))) 

fwrite(rides_2022_adj, file = paste0("processed/rides_", "2022", "_adj.csv"))
rm(rides_2022, rides_2022_adj)









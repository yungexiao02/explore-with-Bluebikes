# Creating rides by date (commented out station merge and month aggregation for now)
## Note: something is wrong where I can't do 2021-2025 together, need to separately do 2024 and 2025 for some reason 

# Load packages 
library(tidyverse)
library(lubridate)

# Set project environment 
setwd("/Users/yungexiao/Desktop/bluebike analysis")

# # Load station data
# stations <- read.csv("data/stations.csv") # stations data 
# station_dup <- stations %>% 
#   filter(station_id == "214") %>% # 2 stations with same ID (214) but different station number
#   select(station_id) %>% 
#   distinct() 

years = seq(2021, 2025, 1) # years string for iterating

for (yr in years){
  
  # Load ride data -- check this at some point
  if (yr %in% c(2021, 2022)){
    filepath = str_c("processed/rides/rides cleaned raw/rides_", yr, "_adj.csv")
    rides <- read.csv(filepath)
  } else {
    filepath = str_c("processed/rides/rides cleaned raw/rides_", yr, ".csv")
    rides <- read.csv(filepath)
  }
  
  # # choose correct duplicate station
  # station_tokeep <- rides %>% 
  #   filter(start_station_id == 214 | start_station_id == "Bremen St at Marion St" | start_station_id == "Airport T Stop - Bremen St at Brooks St") %>% 
  #   select(start_station_name, start_station_id) %>% 
  #   distinct() 
  # 
  # keep = station_tokeep$start_station_name # store station to keep
  # 
  # # de-duplicate stations correctly 
  # stations_tomerge <- stations %>% 
  #   filter(!(!(station_name %in% keep) & station_id == 214)) %>% # remove correct duplicate
  #   select(-X) # select necesary variables for merge

  # # merge ride data wiith stations
  # rides <- rides %>% 
  #   mutate(station_id = as.character(start_station_id),
  #          end_station_id = as.character(end_station_id), # convert to character for merge
  #          started_at = ymd_hms(started_at),
  #          ended_at = ymd_hms(ended_at)) %>% # convert to datetime
  #   left_join(stations_tomerge, by = "station_id") %>% 
  #   select(-station_id) %>% 
  #   rename(start_municipality = station_municipality,
  #          station_id = end_station_id) %>% 
  #   left_join(stations_tomerge, by = "station_id") %>% 
  #   rename(end_municipality = station_municipality) %>% 
  #   select(ride_id, rideable_type, started_at, ended_at, start_station_name, end_station_name, start_municipality, end_municipality, member_casual,
  #          start_station_id, end_station_id, start_station_) 
  
  # add date/time, dow, seasons, duration variables -- flags for weekday and season created based on starting date
  rides <- rides %>% 
    mutate(started_at = as_datetime(started_at),
           ended_at = as_datetime(ended_at)) %>% 
    mutate(start_date = date(started_at),
           start_month = month(started_at),
           start_day = day(started_at),
           start_week = week(started_at),
           start_year = year(started_at),
           start_hour = hour(started_at),
           start_dow = weekdays(started_at),
           end_date = date(ended_at),
           end_month = month(ended_at),
           end_day = day(ended_at),
           end_week = week(ended_at),
           end_year = year(ended_at),
           end_hour = hour(ended_at),
           end_dow = weekdays(ended_at)
    ) %>% 
    mutate(wkday_flag = ifelse(start_dow %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 1, 0),
           season = case_when(
             start_month == 12 & start_day >= 21 | start_month %in% c(1, 2) |
               (start_month == 3 & start_day < 20) ~ "winter",
             start_month == 3 & start_day >= 20 | start_month %in% c(4, 5) |
               (start_month == 6 & start_day < 21) ~ "spring",
             start_month == 6 & start_day >= 21 | start_month %in% c(7, 8) |
               (start_month == 9 & start_day < 22) ~ "summer",
             start_month == 9 & start_day >= 22 | start_month %in% c(10, 11) |
               (start_month == 12 & start_day < 21) ~ "autumn")
    ) %>% 
    mutate(ride_dur_sec =as.numeric(difftime(ended_at, started_at, units = "secs")), # ride duration -- seconds
           ride_dur_min = as.numeric(difftime(ended_at, started_at, units = "mins"))) # ride duration -- minutes
  
  # flag high outlier durations
  dur99 = quantile(rides$ride_dur_min, probs = 0.99) # store 99th percentile of ride duration 
  rides <- rides %>% 
    mutate(outlier_dur = ifelse(ride_dur_min > dur99, 1, 0))
  
  # save csv
  write.csv(rides, str_c("processed/rides/rides flagged/rides_", yr, "_flagged.csv"))
  
  # collapse rides to date-level 
  rides_bydate <- rides %>% 
    mutate(member_flag = ifelse(member_casual == "member", 1, 0),
           ebike_flag = case_when(rideable_type == "electric_bike" ~ 1,
                          rideable_type == "classic_bike" ~ 0, 
                          TRUE ~ NA)) %>% 
    filter(outlier_dur != 1) %>% # drop very high outliers
    group_by(start_date) %>% 
    summarise(n_rides = n(), 
              tot_dur_min = sum(ride_dur_min),
              med_dur_min = median(ride_dur_min),
              mean_dur_min = mean(ride_dur_min),
              season = first(season), 
              p_member_rides = mean(member_flag),
              p_ebike_rides = mean(ebike_flag))
  
  # save csv
  write.csv(rides_bydate, str_c("processed/rides/by date/rides_", yr, "_bydate.csv"))
  
  # # collapse rides to month-level 
  # rides_bymonth <- rides %>% 
  #   mutate(member_flag = ifelse(member_casual == "member", 1, 0),
  #          ebike_flag = case_when(rideable_type == "electric_bike" ~ 1,
  #                                 rideable_type == "classic_bike" ~ 0, 
  #                                 TRUE ~ NA)) %>% 
  #   filter(outlier_dur != 1) %>% # drop very high outliers
  #   group_by(start_month) %>% 
  #   summarise(n_rides = n(), 
  #             n_days = n_distinct(start_day),
  #             tot_dur_min = sum(ride_dur_min), # total ride duration in a given month
  #             med_dur_min = median(ride_dur_min), # median ride duration in a given month
  #             mean_dur_min = mean(ride_dur_min), # average ride duration in a given month 
  #             # season = season, # not consistent over month  
  #             p_member_rides = mean(member_flag), # proportion of member rides in a month
  #             p_ebike_rides = mean(ebike_flag)) %>% # proportion of e-bike rides in a month
  #   mutate(month_name = case_when(start_month == 1 ~ "January",
  #                                 start_month == 2 ~ "February",
  #                                 start_month == 3 ~ "March",
  #                                 start_month == 4 ~ "April",
  #                                 start_month == 5 ~ "May",
  #                                 start_month == 6 ~ "June",
  #                                 start_month == 7 ~ "July",
  #                                 start_month == 8 ~ "August",
  #                                 start_month == 9 ~ "September",
  #                                 start_month == 10 ~ "October",
  #                                 start_month == 11 ~ "November",
  #                                 start_month == 12 ~ "December",
  #                                 TRUE ~ NA))
  
  # save csv
  # write.csv(rides_bymonth, str_c("processed/rides/by month/rides_", yr, "_bymonth.csv"))
}

#-----------------------------------------------------------------------------------------
# APPEND BY DATE
rides_bydate21 = read.csv("processed/rides/by date/rides_2021_bydate.csv") %>% select(-X)
rides_bydate22 = read.csv("processed/rides/by date/rides_2022_bydate.csv") %>% select(-X) 
rides_bydate23 = read.csv("processed/rides/by date/rides_2023_bydate.csv") %>% select(-X)
rides_bydate24 = read.csv("processed/rides/by date/rides_2024_bydate.csv") %>% select(-X) 
rides_bydate25 = read.csv("processed/rides/by date/rides_2025_bydate.csv") %>% select(-X) 

rides_bydate_full <- rbind(rides_bydate21,
      rides_bydate22,
      rides_bydate23,
      rides_bydate24,
      rides_bydate25) %>% 
  mutate(year = year(start_date),
         month = month(start_date),
         month_name = case_when(month == 1 ~ "January",
                                month == 2 ~ "February",
                                month == 3 ~ "March",
                                month == 4 ~ "April",
                                month == 5 ~ "May",
                                month == 6 ~ "June",
                                month == 7 ~ "July",
                                month == 8 ~ "August",
                                month == 9 ~ "September",
                                month == 10 ~ "October",
                                month == 11 ~ "November",
                                month == 12 ~ "December",
                                TRUE ~ NA),
         day = day(start_date),
         dow = wday(start_date),
         dow_name = wday(start_date, label=T),
         med_dur_min = round(med_dur_min, 2),
         mean_dur_min = round(mean_dur_min, 2),
         p_member_rides = round(p_member_rides, 4),
         p_ebike_rides = round(p_ebike_rides, 4))

write.csv(rides_bydate_full, "processed/rides/by date/rides_bydate_full.csv")

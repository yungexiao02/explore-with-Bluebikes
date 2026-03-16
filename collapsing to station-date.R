# Merge with stations data, collapse to station-date and station-year (for StationFinder) level

# Load libraries
library(tidyverse)
library(lubridate)


####################### MERGE DATA PREP #############################

# Stations Data 
stations <- read.csv("data/stations.csv")
colnames(stations)
stations <- stations %>% 
  rename(lat = station_lat,
         lng = station_long, 
         municipality = station_municipality) %>% 
  mutate(station_name = trimws(station_name),
         station_number=trimws(station_number),
         station_id = trimws(station_id)) %>% 
  select(-X)

# stations1 for numeric merge, stations2 for alpha-numeric merge
stations1 <- stations %>% 
  select(-station_number) %>% 
  filter(station_id != "No ID pre-March 2023") %>% 
  arrange(station_id)
stations2 <- stations %>% 
  select(-station_id) %>% 
  rename(station_id = station_number) %>% 
  arrange(station_id)

# check for duplicates 
table(table(stations1$station_id) > 1) # duplicate exists for id = 214
table(table(stations2$station_id) > 1) # no duplicate exists

# 2021-2025 
# Ride-level data, cleaned and flagged 
# rides21 = read.csv("processed/rides/rides flagged/rides_2021_flagged.csv") %>% select(-X)
# rides22 = read.csv("processed/rides/rides flagged/rides_2022_flagged.csv") %>% select(-X)
# rides23 = read.csv("processed/rides/rides flagged/rides_2023_flagged.csv") %>% select(-X)
# rides24 = read.csv("processed/rides/rides flagged/rides_2024_flagged.csv") %>% select(-X)
rides25 = read.csv("processed/rides/rides flagged/rides_2025_flagged.csv") %>% select(-X)

# Check station_id type for each year (changes at some point in 2023 I think)
# head(rides21$start_station_id) # uses # for id 
# head(rides22$start_station_id) # uses # for id
# head(rides23$start_station_id) # uses both for id
# tail(rides23$start_station_id) # uses both for id
# head(rides24$start_station_id) # uses alpha# for id 
# head(rides25$start_station_id) # uses alpha# for id 

# check station_id = 214 
# table(rides21$start_station_name[rides21$start_station_id == "214"]) # Airport T Stop - Bremen St at Brooks St
# table(rides22$start_station_name[rides22$start_station_id == "214"]) # Airport T Stop - Bremen St at Brooks St
# table(rides23$start_station_name[rides23$start_station_id == "214"]) # Airport T Stop - Bremen St at Brooks St
# table(rides24$start_station_name[rides24$start_station_id == "214"]) # none -- good
# table(rides25$start_station_name[rides25$start_station_id == "214"]) # none -- good

# collapse to station-date level for each year 
# station_bydate21 <- rides21 %>%
#   mutate(started_at = as_datetime(started_at),
#          ended_at = as_datetime(ended_at),
#          start_station_id = trimws(start_station_id),
#          end_station_id = trimws(end_station_id)) %>%
#   select(started_at, ended_at, member_casual,
#          start_station_id, end_station_id) %>%
#   # pivot data (twice as long)
#   pivot_longer(cols = contains("station"),
#              names_to = c("point", ".value"),
#              names_pattern = "(start|end)_(.*)") %>%
#   # add flags and other vars -- removed hour level for now and dow flags
#   mutate(use_dt = as_datetime(ifelse(point == "start", as_datetime(started_at),
#                                      ifelse(point == "end", as_datetime(ended_at), NA))),
#          start = ifelse(point == "start", 1, 0),
#          end = ifelse(point == "end", 1, 0),
#          member_removed = ifelse(member_casual == "member" & start == 1, 1, 0),
#          member_returned = ifelse(member_casual == "member" & end == 1, 1, 0),
#          date = date(use_dt),
#          year = year(date)) %>%
#   # select vars of interest
#   select(station_id,
#          use_dt, date, year,
#          start, end, member_removed, member_returned) %>%
#   group_by(station_id, date) %>% # station-date level
#   summarise(year = first(year),
#             bikes_removed = sum(start),
#             bikes_returned = sum(end),
#             total_activity = bikes_removed + bikes_returned,
#             members_removed = sum(member_removed),
#             members_returned = sum(member_returned))
# 
# station_bydate22 <- rides22 %>% 
#   mutate(started_at = as_datetime(started_at),
#          ended_at = as_datetime(ended_at),
#          start_station_id = trimws(start_station_id),
#          end_station_id = trimws(end_station_id)) %>% 
#   select(started_at, ended_at, member_casual, 
#          start_station_id, end_station_id) %>% 
#   # pivot data (twice as long) 
#   pivot_longer(cols = contains("station"), 
#                names_to = c("point", ".value"),
#                names_pattern = "(start|end)_(.*)") %>% 
#   # add flags and other vars -- removed hour level for now and dow flags
#   mutate(use_dt = as_datetime(ifelse(point == "start", as_datetime(started_at),
#                                      ifelse(point == "end", as_datetime(ended_at), NA))),
#          start = ifelse(point == "start", 1, 0),
#          end = ifelse(point == "end", 1, 0),
#          member_removed = ifelse(member_casual == "member" & start == 1, 1, 0),
#          member_returned = ifelse(member_casual == "member" & end == 1, 1, 0),
#          date = date(use_dt),
#          year = year(date)) %>% 
#   # select vars of interest
#   select(station_id, 
#          use_dt, date, year, 
#          start, end, member_removed, member_returned) %>% 
#   group_by(station_id, date) %>% # station-date level
#   summarise(year = first(year),
#             bikes_removed = sum(start),
#             bikes_returned = sum(end),
#             total_activity = bikes_removed + bikes_returned,
#             members_removed = sum(member_removed),
#             members_returned = sum(member_returned)) 

# station_bydate23 <- rides23 %>% 
#   mutate(started_at = as_datetime(started_at),
#          ended_at = as_datetime(ended_at),
#          start_station_id = trimws(start_station_id),
#          end_station_id = trimws(end_station_id)) %>% 
#   select(started_at, ended_at, member_casual, 
#          start_station_id, end_station_id) %>% 
#   # pivot data (twice as long) 
#   pivot_longer(cols = contains("station"), 
#                names_to = c("point", ".value"),
#                names_pattern = "(start|end)_(.*)") %>% 
#   # add flags and other vars -- removed hour level for now and dow flags
#   mutate(use_dt = as_datetime(ifelse(point == "start", as_datetime(started_at),
#                                      ifelse(point == "end", as_datetime(ended_at), NA))),
#          start = ifelse(point == "start", 1, 0),
#          end = ifelse(point == "end", 1, 0),
#          member_removed = ifelse(member_casual == "member" & start == 1, 1, 0),
#          member_returned = ifelse(member_casual == "member" & end == 1, 1, 0),
#          date = date(use_dt),
#          year = year(date)) %>% 
#   # select vars of interest
#   select(station_id, 
#          use_dt, date, year, 
#          start, end, member_removed, member_returned) %>% 
#   group_by(station_id, date) %>% # station-date level
#   summarise(year = first(year),
#             bikes_removed = sum(start),
#             bikes_returned = sum(end),
#             total_activity = bikes_removed + bikes_returned,
#             members_removed = sum(member_removed),
#             members_returned = sum(member_returned)) 

# station_bydate24 <- rides24 %>%
#   mutate(started_at = as_datetime(started_at),
#          ended_at = as_datetime(ended_at),
#          start_station_id = trimws(start_station_id),
#          end_station_id = trimws(end_station_id)) %>%
#   select(started_at, ended_at, member_casual,
#          start_station_id, end_station_id) %>%
#   # pivot data (twice as long)
#   pivot_longer(cols = contains("station"),
#                names_to = c("point", ".value"),
#                names_pattern = "(start|end)_(.*)") %>%
#   # add flags and other vars -- removed hour level for now and dow flags
#   mutate(use_dt = as_datetime(ifelse(point == "start", as_datetime(started_at),
#                                      ifelse(point == "end", as_datetime(ended_at), NA))),
#          start = ifelse(point == "start", 1, 0),
#          end = ifelse(point == "end", 1, 0),
#          member_removed = ifelse(member_casual == "member" & start == 1, 1, 0),
#          member_returned = ifelse(member_casual == "member" & end == 1, 1, 0),
#          date = date(use_dt),
#          year = year(date)) %>%
#   # select vars of interest
#   select(station_id,
#          use_dt, date, year,
#          start, end, member_removed, member_returned) %>%
#   group_by(station_id, date) %>% # station-date level
#   summarise(year = first(year),
#             bikes_removed = sum(start),
#             bikes_returned = sum(end),
#             total_activity = bikes_removed + bikes_returned,
#             members_removed = sum(member_removed),
#             members_returned = sum(member_returned))

station_bydate25 <- rides25 %>% 
  mutate(started_at = as_datetime(started_at),
         ended_at = as_datetime(ended_at),
         start_station_id = trimws(start_station_id),
         end_station_id = trimws(end_station_id)) %>% 
  select(started_at, ended_at, member_casual, 
         start_station_id, end_station_id) %>% 
  # pivot data (twice as long) 
  pivot_longer(cols = contains("station"), 
               names_to = c("point", ".value"),
               names_pattern = "(start|end)_(.*)") %>% 
  # add flags and other vars -- removed hour level for now and dow flags
  mutate(use_dt = as_datetime(ifelse(point == "start", as_datetime(started_at),
                                     ifelse(point == "end", as_datetime(ended_at), NA))),
         start = ifelse(point == "start", 1, 0),
         end = ifelse(point == "end", 1, 0),
         member_removed = ifelse(member_casual == "member" & start == 1, 1, 0),
         member_returned = ifelse(member_casual == "member" & end == 1, 1, 0),
         date = date(use_dt),
         year = year(date)) %>% 
  # select vars of interest
  select(station_id, 
         use_dt, date, year, 
         start, end, member_removed, member_returned) %>% 
  group_by(station_id, date) %>% # station-date level
  summarise(year = first(year),
            bikes_removed = sum(start),
            bikes_returned = sum(end),
            total_activity = bikes_removed + bikes_returned,
            members_removed = sum(member_removed),
            members_returned = sum(member_returned)) 

# merge with stations1 or stations2 
# 
## 2021
# merged21 <- station_bydate21 %>%
#   left_join(stations1 %>%
#               filter(station_name != "Bremen St at Marion St"))
# 
# missing_stations21 <- merged21 %>% filter(is.na(station_name))
# stations_byyear21 <- merged21 %>%
#   filter(!is.na(station_name)) %>%
#   group_by(station_name, year) %>%
#   summarise(total_docks = first(station_tot_docks),
#             szn_status = first(station_szn_status),
#             lat = first(lat),
#             lng = first(lng),
#             municipality = first(municipality),
#             avg_removed = mean(bikes_removed),
#             avg_returned = mean(bikes_returned),
#             avg_removed_per_dock = mean(bikes_removed)/total_docks,
#             avg_returned_per_dock = mean(bikes_returned)/total_docks,
#             avg_tot_activity = mean(total_activity),
#             avg_tot_activity_per_dock = mean(total_activity)/total_docks, # prioritize plotting this one
#             pct_removed_gt_returned = mean(bikes_removed > bikes_returned), # prioritize plotting this one
#   ) %>%
#   arrange(year, station_name)
# 
# ## 2022 
# merged22 <- station_bydate22 %>% 
#   left_join(stations1 %>% 
#               filter(station_name != "Bremen St at Marion St"))
# 
# missing_stations22 <- merged22 %>% filter(is.na(station_name))
# 
# stations_byyear22 <- merged22 %>% 
#   filter(!is.na(station_name)) %>% 
#   group_by(station_name, year) %>% 
#   summarise(total_docks = first(station_tot_docks),
#             szn_status = first(station_szn_status),
#             lat = first(lat),
#             lng = first(lng),
#             municipality = first(municipality),
#             avg_removed = mean(bikes_removed),
#             avg_returned = mean(bikes_returned),
#             avg_removed_per_dock = mean(bikes_removed)/total_docks, 
#             avg_returned_per_dock = mean(bikes_returned)/total_docks, 
#             avg_tot_activity = mean(total_activity),
#             avg_tot_activity_per_dock = mean(total_activity)/total_docks, # prioritize plotting this one
#             pct_removed_gt_returned = mean(bikes_removed > bikes_returned), # prioritize plotting this one
#   ) %>% 
#   arrange(year, station_name)
# 
# ## 2023 -- partly merge with stations1
# missing_bydate23 <- station_bydate23 %>% 
#   filter(station_id == "") # some missing data -- no station identified; I don't believe it's an issue with the collapse 
# 
# station_bydate23_1 <- station_bydate23 %>% 
#   filter(!str_detect(station_id, "[a-zA-Z]") & station_id != "")
# station_bydate23_2 <- station_bydate23 %>% 
#   filter(str_detect(station_id, "[a-zA-Z]") & station_id != "")
# 
# ### 2023 with stations1 
# merged23_1 <- station_bydate23_1 %>% 
#   left_join(stations1 %>% 
#               filter(station_name != "Bremen St at Marion St"))
# 
# missing_stations23_1 <- merged23_1 %>% filter(is.na(station_name))
# 
# stations_byyear23_1 <- merged23_1 %>% 
#   filter(!is.na(station_name)) %>% 
#   group_by(station_name, year) %>% 
#   summarise(total_docks = first(station_tot_docks),
#             szn_status = first(station_szn_status),
#             lat = first(lat),
#             lng = first(lng),
#             municipality = first(municipality),
#             avg_removed = mean(bikes_removed),
#             avg_returned = mean(bikes_returned),
#             avg_removed_per_dock = mean(bikes_removed)/total_docks, 
#             avg_returned_per_dock = mean(bikes_returned)/total_docks, 
#             avg_tot_activity = mean(total_activity),
#             avg_tot_activity_per_dock = mean(total_activity)/total_docks, # prioritize plotting this one
#             pct_removed_gt_returned = mean(bikes_removed > bikes_returned), # prioritize plotting this one
#   ) %>% 
#   arrange(year, station_name)
# 
# merged23_2 <- stations_bydate23_2 %>% 
#   left_join(stations2)
# 
# missing_stations23_2 <- merged23_2 %>% filter(is.na(station_name))
# 
# stations_byyear23_2 <- merged23_2 %>% 
#   filter(!is.na(station_name)) %>% 
#   group_by(station_name, year) %>% 
#   summarise(total_docks = first(station_tot_docks),
#             szn_status = first(station_szn_status),
#             lat = first(lat),
#             lng = first(lng),
#             municipality = first(municipality),
#             avg_removed = mean(bikes_removed),
#             avg_returned = mean(bikes_returned),
#             avg_removed_per_dock = mean(bikes_removed)/total_docks, 
#             avg_returned_per_dock = mean(bikes_returned)/total_docks, 
#             avg_tot_activity = mean(total_activity),
#             avg_tot_activity_per_dock = mean(total_activity)/total_docks, # prioritize plotting this one
#             pct_removed_gt_returned = mean(bikes_removed > bikes_returned), # prioritize plotting this one
#   ) %>% 
#   arrange(year, station_name)
# 
# # colnames(missing_stations23_1); colnames(missing_stations23_2)
# missing_stations23 <- rbind(missing_stations23_1, missing_stations23_2)
# stations_byyear23 <- rbind(stations_byyear23_1, stations_byyear23_2)

# ## 2024
# 
# merged24 <- station_bydate24 %>%
#   left_join(stations2)
# 
# missing_stations24 <- merged24 %>% filter(is.na(station_name))
# 
# stations_byyear24 <- merged24 %>%
#   filter(!is.na(station_name)) %>%
#   group_by(station_name, year) %>%
#   summarise(total_docks = first(station_tot_docks),
#             szn_status = first(station_szn_status),
#             lat = first(lat),
#             lng = first(lng),
#             municipality = first(municipality),
#             avg_removed = mean(bikes_removed),
#             avg_returned = mean(bikes_returned),
#             avg_removed_per_dock = mean(bikes_removed)/total_docks,
#             avg_returned_per_dock = mean(bikes_returned)/total_docks,
#             avg_tot_activity = mean(total_activity),
#             avg_tot_activity_per_dock = mean(total_activity)/total_docks, # prioritize plotting this one
#             pct_removed_gt_returned = mean(bikes_removed > bikes_returned), # prioritize plotting this one
#   ) %>%
#   arrange(year, station_name)

## 2025

merged25 <- station_bydate25 %>%
left_join(stations2)

missing_stations25 <- merged25 %>% filter(is.na(station_name))

stations_byyear25 <- merged25 %>%
  filter(!is.na(station_name)) %>%
  group_by(station_name, year) %>%
  summarise(total_docks = first(station_tot_docks),
            szn_status = first(station_szn_status),
            lat = first(lat),
            lng = first(lng),
            municipality = first(municipality),
            avg_removed = mean(bikes_removed),
            avg_returned = mean(bikes_returned),
            avg_removed_per_dock = mean(bikes_removed)/total_docks,
            avg_returned_per_dock = mean(bikes_returned)/total_docks,
            avg_tot_activity = mean(total_activity),
            avg_tot_activity_per_dock = mean(total_activity)/total_docks, # prioritize plotting this one
            pct_removed_gt_returned = mean(bikes_removed > bikes_returned), # prioritize plotting this one
  ) %>%
  arrange(year, station_name)


# save station-year data for StationFinder app 
# saveRDS(stations_byyear21, "processed/stations/by year/stations_byyear_2021.rds")
# saveRDS(stations_byyear22, "processed/stations/by year/stations_byyear_2022.rds")
# saveRDS(stations_byyear23, "processed/stations/by year/stations_byyear_2023.rds")
# saveRDS(stations_byyear24, "processed/stations/by year/stations_byyear_2024.rds")
saveRDS(stations_byyear25, "processed/stations/by year/stations_byyear_2025.rds")

# save missing stations in case
# saveRDS(missing_stations21, "processed/stations/by year/missingstations_byyear_2021.rds")
# saveRDS(missing_stations22, "processed/stations/by year/missingstations_byyear_2022.rds")
# saveRDS(missing_stations23, "processed/stations/by year/missingstations_byyear_2023.rds")
# saveRDS(missing_stations24, "processed/stations/by year/missingstations_byyear_2024.rds")
saveRDS(missing_stations25, "processed/stations/by year/missingstations_byyear_2025.rds")


















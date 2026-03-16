# Download and unzip Bluebike data from 2021-2025

## Create vector of URLs
url_head = "https://s3.amazonaws.com/hubway-data/"
url_tail = "-bluebikes-tripdata.zip"
url_yr = c("2021", "2022", "2023", "2024", "2025")
url_month = sprintf("%02d", 1:12) # or c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

urls <- character() # initialize url vector

for (yr in url_yr) {
  for (mo in url_month) {
    urls <- c(urls, paste0(url_head, yr, mo, url_tail))
  }
}

length(urls) # check -- 60, good 

library(tools) # for unzipping

data_dir <- "data"
dir.create(data_dir, showWarnings = FALSE)
archive_dir <- "data/archive"
dir.create(archive_dir, showWarnings = FALSE) # create folder in project folder called "raw data"
down_dir <- "data/archive/downloads"
dir.create(down_dir, showWarnings = FALSE) # create folder in project folder called "raw data"

# Note: 202511 was weirdly named, manually downlaoded data 
for (u in urls) {
  name <- file_path_sans_ext(basename(u)) # basename drops everything except filename (excludes .zip too)
  unzip_dir <- file.path(down_dir, name)
  dir.create(unzip_dir, showWarnings = FALSE)
  
  message("Processing: ", name)
  
  zip_file <- tempfile(fileext = ".zip")
  
  tryCatch({
    download.file(u, zip_file, mode = "wb", quiet = TRUE)
    unzip(zip_file, exdir = unzip_dir)
  }, error = function(e) {
    message("Failed: ", u)
  })
}

csv_dir = "data/ride data extracts"
dir.create(csv_dir, showWarnings = F)
csv_files <- list.files(down_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
file.copy(csv_files, csv_dir) # copy all 60 unzipped .csv files from downloads to data extracts

station_csv <- read.csv(station_url, header = T)
names(station_csv) <- c("station_number", "station_name", "station_lat", "station_long", "station_szn_status", "station_municipality", "station_tot_docks", "station_id")
station_csv <- station_csv[-1, ]
write.csv(station_csv, paste("data/stations.csv"))

#===============================================================================
# Extract WATLAS data from database
#===============================================================================

# This script extracts WATLAS data from the database and saves it as a CSV file.
# Afterwards check the data (0c_check_raw_data).

# It extracts all data from one season and saves then separately for each tag
# as csv.

# packages
library(tools4watlas)
library(foreach)

# specify season and file path's
season_id <- 2023
season2_id <- 2024 # set to NULL if not existing yet

# file path to WATLAS teams data folder
watlas_fp <- atl_file_path("watlas_teams")

# file path to sqlite databases
db_fp <- "C:/Users/jkrietsch/OneDrive - NIOZ/Documents/watlas_data/localizations/"

#-------------------------------------------------------------------------------
# Load WATLAS data
#-------------------------------------------------------------------------------

# load Excel file with metadata
all_tags <- readxl::read_excel(
  paste0(watlas_fp, "tags/tags_watlas_all.xlsx"),
  sheet = "tags_watlas_all"
) |>
  data.table()

# check species names
all_tags[, .N, species]
all_tags[season == season_id, .N, species]

# subset desired tags using data.table
tags <- all_tags[season == season_id & !is.na(species)]$tag

# check N
tags |> length()

# time period for which data should be extracted form the database
from <- paste0(season_id, "-01-01 00:00:00")
to <- paste0(season_id, "-12-31 23:59:59")

# database connection
sqlite_db <- paste0(db_fp, "watlas-", season_id, ".sqlite")
con <- RSQLite::dbConnect(RSQLite::SQLite(), sqlite_db)

# load data from database
data <- atl_get_data(
  tags,
  tracking_time_start = from,
  tracking_time_end = to,
  timezone = "CET",
  use_connection = con
)

# close connection
RSQLite::dbDisconnect(con)

### add data from next season if available

if (!is.null(season2_id)) {

  # time period for which data should be extracted form the database
  from <- paste0(season2_id, "-01-01 00:00:00")
  to <- paste0(season2_id, "-12-31 23:59:59")

  # database connection
  sqlite_db <- paste0(db_fp, "watlas-", season2_id, ".sqlite")
  con <- RSQLite::dbConnect(RSQLite::SQLite(), sqlite_db)

  # load data from database
  data2 <- atl_get_data(
    tags,
    tracking_time_start = from,
    tracking_time_end = to,
    timezone = "CET",
    use_connection = con
  )

  # close connection
  RSQLite::dbDisconnect(con)

  # combine data
  data <- rbind(data, data2)

}

# join with species data
all_tags[, tag := as.character(tag)]
data[all_tags, on = "tag", `:=`(species = i.species)]

# make species first column
setcolorder(data, c("species", setdiff(names(data), c("species"))))

# order data.table
setorder(data, species, tag, time)

#-------------------------------------------------------------------------------
# Save split by tag
#-------------------------------------------------------------------------------

# unique ID (here by tag)
id <- unique(data$tag)

# split data (only necessary if dataset is to big to send to all cores)
foreach(i = id) %do% {

  # subset data
  data_subset <- data[tag == i]

  # save data
  fwrite(
    data_subset,
    paste0(
      "./data/", season_id, "/split_raw/watlas_", season_id,
      "_raw_tag_", i, ".csv"
    ),
    yaml = TRUE
  )

}

#===============================================================================
# Check last data of one tag
#===============================================================================

# This script is there to check manually more in detail the data of one tag.

# Summary
# 1. Load data from one tag
# 2. Check data from one tag

# packages
library(tools4watlas)
library(ggplot2)
library(scales)
library(viridis)
library(foreach)
library(mapview)

# specify season and tag
season_id <- 2023
tag_id <- "3101"

# file path to SQLite databases
db_fp <- atl_file_path("sqlite_db")

#-------------------------------------------------------------------------------
# 1. Load data from one tag
#-------------------------------------------------------------------------------

# time period for which data should be extracted form the database
from <- paste0(season_id, "-01-01 00:00:00")
to <- paste0(season_id, "-12-31 23:59:59")

# database connection
sqlite_db <- paste0(db_fp, "watlas-", season_id, ".sqlite")
con <- RSQLite::dbConnect(RSQLite::SQLite(), sqlite_db)

# load data from database
data <- atl_get_data(
  tag_id,
  tracking_time_start = from,
  tracking_time_end = to,
  timezone = "CET",
  use_connection = con
)

# close connection
RSQLite::dbDisconnect(con)

#-------------------------------------------------------------------------------
# 2. Check data from one tag
#-------------------------------------------------------------------------------

# plot all by datetime
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE
)

# plot all by gap
atl_check_tag(
  data,
  option = "gap", scale_trans = "log",
  highlight_first = TRUE, highlight_last = TRUE
)

# plot last n positions
atl_check_tag(
  data,
  option = "datetime",
  highlight_first = TRUE, highlight_last = TRUE, last_n = 1000
)

# subset last n positions
data_subset <- data[max(1, .N - 999):.N]

# calculate time from last in hours
data_subset[, time_from_last := (time - max(time)) / 60 / 60, by = tag]

# round all numeric columns to 1 decimal place
data_subset[, (names(data_subset)) := lapply(
  .SD, function(x) if (is.numeric(x)) round(x, 1) else x
)]

# make data spatial
d_sf <- atl_as_sf(
  data_subset,
  additional_cols = c("datetime", "time_from_last", "nbs")
)

# add track
d_sf_lines <- atl_as_sf(
  data_subset,
  additional_cols = c("time_from_last"),
  option = "lines"
)

# plot interactive map
mapview(d_sf_lines, zcol = "time_from_last", legend = FALSE) +
  mapview(d_sf, zcol = "time_from_last")

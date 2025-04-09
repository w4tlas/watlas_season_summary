#===============================================================================
# Check raw WATLAS data
#===============================================================================

# This script gives and overview of the data and checks for potential issues,
# like fallen off tags / dead birds. Each chunk is independent.

# Summary
# 1. Overview of the data
# 2. Overview of all data by species
# 3. Check all data with map by ID
# 4. Check all data with map by ID for last n positions

# packages
library(tools4watlas)
library(ggplot2)
library(scales)
library(viridis)
library(foreach)
library(doFuture)

# specify season and file path's
season_id <- 2023

# list all files
file_list <- list.files(
  path = paste0("./data/", season_id, "/split_raw/"), full.names = TRUE
)

# read and combine all CSV files into one data table
data <- rbindlist(lapply(file_list, fread, yaml = TRUE))

#-------------------------------------------------------------------------------
# 1. Overview of all data
#-------------------------------------------------------------------------------

# data summary
data_summary <- atl_summary(data, id_columns = c("species", "tag"))

# set order
setorder(data_summary, species, tag)

# save summary
# fwrite(data_summary, "./data/metadata/all_shorebirds_morphometrics.csv", yaml = TRUE)

# N individuals with tagging data
data_summary |> nrow()

# N by species
data_summary[, .N, by = species]

# plot the number of positions per day
# add date
data[, date := as.Date(datetime)] |> invisible()

# N positions by species and day
data_subset <- data[, .N, by = .(species, tag, date)]

# plot data
ggplot(data_subset, aes(x = date, y = tag, fill = N)) +
  geom_tile() +
  scale_fill_viridis(
    option = "A", discrete = FALSE, trans = "log10", name = "N positions",
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    direction = -1
  ) +
  labs(x = "Date", y = "Tag", title = season_id) +
  theme_classic() +
  facet_grid(species ~ ., scales = "free_y", space = "free", switch = "y") +
  theme(
    axis.title.y = element_blank(),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside"
  )

# save
ggsave(
  paste0("./outputs/season_overview/data_by_day_all_", season_id, ".png"),
  plot = last_plot(), width = 3240, height = 2160, units = c("px"), dpi = 300
)

# plot overview of the data
# create basemap
bm <- atl_create_bm(data, buffer = 800)

# round data to 1 ha (100x100 meter) grid cells
data[, c("x_round", "y_round") := list(
  plyr::round_any(x, 100),
  plyr::round_any(y, 100)
)]

# N by location
data_subset <- data[, .N, by = c("x_round", "y_round")]

# plot heatmap
bm +
  geom_tile(
    data = data_subset, aes(x_round, y_round, fill = N),
    linewidth = 0.1, show.legend = TRUE
  ) +
  scale_fill_viridis(
    option = "A", discrete = FALSE, trans = "log10", name = "N positions",
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    direction = -1
  ) +
  ggtitle(label = season_id)

# save
ggsave(
  paste0("./outputs/season_overview/heatmap_all_", season_id, ".png"),
  plot = last_plot(), width = 3240, height = 2160, units = c("px"), dpi = 300
)

#-------------------------------------------------------------------------------
# 2. Overview of all data by species
#-------------------------------------------------------------------------------

# unique ID (here by tag)
id <- unique(data$species)

# loop to make plots by day by species
foreach(i = id) %do% {
  
  # subset data
  data_subset <- data[species == i]
  
  # N positions by species and day
  data_subset <- data_subset[, .N, by = .(species, tag, date)]
  
  # plot data
  ggplot(data_subset, aes(x = date, y = tag, fill = N)) +
    geom_tile() +
    scale_fill_viridis(
      option = "A", discrete = FALSE, trans = "log10", name = "N positions",
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x)),
      direction = -1
    ) +
    labs(x = "Date", y = "Tag", title = season_id) +
    theme_classic() +
    facet_grid(species ~ ., scales = "free_y", space = "free", switch = "y") +
    theme(
      axis.title.y = element_blank(),
      strip.text.y.left = element_text(angle = 0),
      strip.placement = "outside"
    )
  
  # save
  ggsave(
    paste0(
      "./outputs/season_overview/by_species/data_by_day_", i,
      "_", season_id, ".png"
    ),
    plot = last_plot(), width = 3240, height = 2160, units = c("px"), dpi = 300
  )
  
}

# loop to make maps by species
foreach(i = id) %do% {
  
  # subset data
  data_subset <- data[species == i]
  
  # create basemap
  bm <- atl_create_bm(data_subset, buffer = 800)
  
  # round data to 1 ha (100x100 meter) grid cells
  data_subset[, c("x_round", "y_round") := list(
    plyr::round_any(x, 100),
    plyr::round_any(y, 100)
  )]
  
  # N by location
  data_subset2 <- data_subset[, .N, by = c("x_round", "y_round")]
  
  # plot heatmap
  bm +
    geom_tile(
      data = data_subset2, aes(x_round, y_round, fill = N),
      linewidth = 0.1, show.legend = TRUE
    ) +
    scale_fill_viridis(
      option = "A", discrete = FALSE, trans = "log10", name = "N positions",
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x)),
      direction = -1
    ) +
    ggtitle(label = season_id)
  
  # save
  ggsave(
    paste0(
      "./outputs/season_overview/by_species/heatmap_", i,
      "_", season_id, ".png"
    ),
    plot = last_plot(), width = 3240, height = 2160, units = c("px"), dpi = 300
  )
  
}

#-------------------------------------------------------------------------------
# 3. Check all data with map by ID
#-------------------------------------------------------------------------------

# Load extracted raw data
data <- fread("./data/localizations/watlas_2023_raw.csv", yaml = TRUE)

# file path
path <- "./outputs/checks/maps_by_tag_raw/"

# unique ID (here by tag)
id <- unique(data$tag)

# split data (only necessary if dataset is to big to send to all cores)
foreach(i = id) %do% {

  # subset data
  data_subset <- data[tag == i]

  # save data
  fwrite(
    data_subset,
    paste0("./data/localizations/split_raw/watlas_2023_raw_", i, ".csv"),
    yaml = TRUE
  )

}

# register cores and backend for parallel processing
registerDoFuture()
plan(multisession)

# loop to make plots for all
foreach(i = id) %dofuture% {

  # subset data
  data_subset <- fread(
    paste0("./data/localizations/split_raw/watlas_2023_raw_", i, ".csv"),
    yaml = TRUE
  )

  # plot and save data
  atl_check_tag(
    data_subset,
    option = "datetime",
    highlight_first = TRUE, highlight_last = TRUE,
    filename = paste0(path, data_subset[1]$species, "_tag_", i)
  )

}

# close parallel workers
plan(sequential)

#-------------------------------------------------------------------------------
# 4. Check all data with map by ID for last n positions
#-------------------------------------------------------------------------------

# Load extracted raw data
data <- fread("./data/localizations/watlas_2023_raw.csv", yaml = TRUE)

# file path
path <- "./outputs/checks/maps_by_tag_raw_last1000/"

# unique ID (here by tag)
id <- unique(data$tag)

# loop to make plots for all
foreach(i = id) %dofuture% {

  # subset data
  data_subset <- fread(
    paste0("./data/localizations/split_raw/watlas_2023_raw_", i, ".csv"),
    yaml = TRUE
  )

  # plot and save data
  atl_check_tag(
    data_subset,
    option = "datetime",
    highlight_first = TRUE, highlight_last = TRUE, last_n = 1000,
    filename = paste0(path, data_subset[1]$species, "_tag_", i)
  )

}

# close parallel workers
plan(sequential)

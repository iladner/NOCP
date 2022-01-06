emlab_data_dir <- "~/juanmayorga@ucsb.edu - Google Drive/Shared drives/emlab/data"

gocp_project_dir <- "~/juanmayorga@ucsb.edu - Google Drive/Shared drives/emlab/projects/current-projects/ocean-conservation-priorities"

source(file.path(emlab_data_dir, "ocean-conservation-priorities","functions", "food_provision_foos.R"))

load(file.path(emlab_data_dir, "ocean-conservation-priorities","inputs", "national_analyses","common_inputs.RData"))

ocean_low_res_moll <- raster::raster(file.path(emlab_data_dir, "ocean-conservation-priorities", "inputs", "ocean_low_res_moll.tif"))

global_cols <- viridis::viridis(n = 8)

global_breaks <- c(0, 0.3, 0.5, 0.7, 0.8, 0.85, 0.9, 0.95, 1)

source("_get_curves.R")

source("_mapping_foos.R")

world_eezs <- sf::st_read(file.path(emlab_data_dir, "marine-regions-EEZ-land-union-v3-202003", "EEZ_Land_v3_202030.shp")) %>%
  sf::st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE) %>%
  janitor::clean_names()

world_eezs_info <- world_eezs %>%
  st_drop_geometry()

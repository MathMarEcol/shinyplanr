library(tidyverse)

# Test stuff
micro_eez <- oceandatr::get_area("Micronesia")
micro_proj <- 'PROJCS["ProjWiz_Custom_Cylindrical_Equal_Area",
                     GEOGCS["GCS_WGS_1984",
                            DATUM["D_WGS_1984",
                                  SPHEROID["WGS_1984",6378137.0,298.257223563]],
                            PRIMEM["Greenwich",0.0],
                            UNIT["Degree",0.0174532925199433]],
                     PROJECTION["Cylindrical_Equal_Area"],
                     PARAMETER["False_Easting",0.0],
                     PARAMETER["False_Northing",0.0],
                     PARAMETER["Central_Meridian",150.5],
                     PARAMETER["Standard_Parallel_1",7],
                     UNIT["Meter",1.0]]'

# micro_proj <- "ESRI:54009"

planning_grid <- oceandatr::get_planning_grid(area_polygon = micro_eez,
                                                 projection_crs = micro_proj,
                                                 resolution = 20000)

micro_eez_projected <- micro_eez %>%
  sf::st_transform(crs = micro_proj) %>%
  sf::st_geometry()


# ggplot() +
#   geom_sf(data = planning_grid, fill = NA) +
#   geom_sf(data = micro_eez, fill = NA, colour = "red")


# bathymetry <- oceandatr::get_bathymetry(planning_grid = planning_grid, keep = FALSE)
# geomorphology <- oceandatr::get_geomorphology(planning_grid = planning_grid)
# knolls <- oceandatr::get_knolls(planning_grid = planning_grid)
# seamounts <- oceandatr::get_seamounts_buffered(planning_grid = planning_grid, buffer = 30000)
# coral_habitat <- oceandatr::get_coral_habitat(planning_grid = planning_grid)
# enviro_regions <- oceandatr::get_enviro_regions(planning_grid = planning_grid, max_num_clusters = 5)



r <- c(oceandatr::get_bathymetry(planning_grid = planning_grid, keep = FALSE),
       oceandatr::get_geomorphology(planning_grid = planning_grid),
       oceandatr::get_knolls(planning_grid = planning_grid),
       oceandatr::get_seamounts_buffered(planning_grid = planning_grid, buffer = 30000),
       oceandatr::get_coral_habitat(planning_grid = planning_grid),
       oceandatr::get_enviro_regions(planning_grid = planning_grid, max_num_clusters = 5)
)

dat_sf <- r %>%
  terra::as.polygons(round = FALSE, aggregate = FALSE,
                     values = TRUE, na.rm = FALSE,
                     na.all = TRUE, extent = FALSE,
                     digits = 10, crs = "micro_proj") %>% # Convert to SpatVector
  sf::st_as_sf() %>% # Convert to sf
  dplyr::mutate(across(everything(), ~replace_na(.x, 0))) %>% # Replace NA/NaN with 0
  dplyr::mutate(Cost_None = 0.1,
                Cost_Random = runif(dim(.)[1]))


saveRDS(dat_sf, file.path("data-raw", "Kosrae", "KosraeTestData.rds"))

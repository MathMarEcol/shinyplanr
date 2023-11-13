library(tidyverse)


# Test stuff
micro_eez <- offshoredatr::get_area("Micronesia")
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

planning_grid <- offshoredatr::get_planning_grid(area_polygon = micro_eez, projection_crs = micro_proj, resolution_km = 20)

micro_eez_projected <- micro_eez %>%
  sf::st_transform(crs = micro_proj) %>%
  sf::st_geometry()


# ggplot() +
#   geom_sf(data = planning_grid, fill = NA) +
#   geom_sf(data = micro_eez, fill = NA, colour = "red")


bathymetry <- offshoredatr::get_bathymetry(area_polygon = micro_eez, keep = FALSE)

geomorphology <- offshoredatr::get_geomorphology(area_polygon = micro_eez, planning_grid = planning_grid)

knolls <- offshoredatr::get_knolls(area_polygon = micro_eez, planning_grid = planning_grid)

seamounts <- offshoredatr::get_seamounts_buffered(area_polygon = micro_eez, planning_grid = planning_grid, buffer_km = 30)

# coral_habitat <- offshoredatr::get_coral_habitat(area_polygon = micro_eez, planning_grid = planning_grid)

enviro_regions <- offshoredatr::get_enviro_regions(area_polygon = micro_eez, planning_grid = planning_grid, max_num_clusters = 5)

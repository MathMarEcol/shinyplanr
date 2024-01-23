library(tidyverse)
library(spatialplanr)
library(oceandatr)

# TODO Fix interp script to work with data files only. Check Jason's version

## TEST CASE
# cCRS <- "ESRI:54009"
#
# eez <- oceandatr::get_area("Australia") %>%
#   sf::st_transform(crs = cCRS)
#
# PUs <- oceandatr::get_planning_grid(area_polygon = eez,
#                                     projection_crs = cCRS,
#                                     option = "sf_hex",
#                                     resolution = 20000)
# ggplot() +
#    geom_sf(data = PUs, fill = NA) +
#   geom_sf(data = eez, fill = NA, colour = "red")


# Organise Planning Grid --------------------------------------------------

proj <- 'PROJCS["ProjWiz_Custom_Cylindrical_Equal_Area",
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

# proj <- "ESRI:54009"

# Get eez to create grid
eez <- oceandatr::get_area("Micronesia") %>%
  sf::st_transform(crs = proj) %>%
  sf::st_geometry() %>%
  sf::st_sf()

# TODO is there a better way to do the code below
# TODO Chat to JF about get_area which returns a multipolygon.
# How do they deal with this when creating the Planning grid.
# It looks like they look at overlap between the centroid and the polygon
# but it doesn't seem to work for the FSM example. There are PUs on land....

# Separate Boundary and Coastline
temp <- eez %>%
  sf::st_cast(to = "POLYGON") %>%
  dplyr::mutate(Area_km2 = sf::st_area(.) %>%
                  units::set_units("km2") %>%
                  units::drop_units())

coast <- temp %>%
  dplyr::filter(Area_km2 < max(Area_km2)) %>%
  dplyr::select(-Area_km2)

bndry <- temp %>%
  dplyr::filter(Area_km2 == max(Area_km2)) %>%
  dplyr::select(-Area_km2)

rm(temp)


PUs <- oceandatr::get_planning_grid(area_polygon = eez,
                                    projection_crs = proj,
                                    option = "sf_hex",
                                    resolution = 20000) %>%
  dplyr::rename(geometry = x) # Temp fix while oceandatr return x


# Check data
ggplot() +
  geom_sf(data = PUs, fill = NA) +
  geom_sf(data = eez, fill = NA, colour = "red")


# Compile datasets -------------------------------------------------------------

dat_sf <- bind_cols(
  oceandatr::get_bathymetry(planning_grid = PUs, keep = FALSE) %>% sf::st_drop_geometry(),
  oceandatr::get_geomorphology(planning_grid = PUs) %>% sf::st_drop_geometry(),
  oceandatr::get_knolls(planning_grid = PUs) %>% sf::st_drop_geometry(),
  oceandatr::get_seamounts_buffered(planning_grid = PUs, buffer = 30000) %>% sf::st_drop_geometry(),
  oceandatr::get_coral_habitat(planning_grid = PUs) %>% sf::st_drop_geometry(),
  oceandatr::get_enviro_regions(planning_grid = PUs, max_num_clusters = 5)
) %>%
  dplyr::mutate(across(everything(), ~replace_na(.x, 0))) %>%  # Replace NA/NaN with 0
  dplyr::rename(geometry = x) # Temp fix while oceandatr return x



# Add cost data -----------------------------------------------------------

cost <- PUs %>%
  splnr_get_distCoast(custom_coast = coast) # Distance to nearest coast

# Locked in areas ---------------------------------------------------------

# TODO These are only point MPAs. Need some polygons to do this right.

lock_in <- "Micronesia (Federated States of)" %>%
  lapply(wdpar::wdpa_fetch,
         wait = TRUE,
         download_dir = rappdirs::user_data_dir("wdpar")
  ) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(.data$MARINE > 0) %>%
  sf::st_transform(crs = proj)


#%>%
  # oceandatr::data_to_planning_grid(planning_grid = PUs, dat = .) %>%
  # dplyr::mutate(data = as.logical(data)) %>%
  # dplyr::rename(locked_in = data)

# Save raw data -----------------------------------------------------------

dat_sf <- bind_cols(dat_sf,
                    lock_in %>% sf::st_drop_geometry(),
                    cost %>% sf::st_drop_geometry()) %>%
  dplyr::relocate(geometry, .after = tidyselect::everything())

saveRDS(dat_sf, file.path("data-raw", "FSM", "FSM_TestData.rds"))

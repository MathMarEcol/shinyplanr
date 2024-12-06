library(tidyverse)
library(spatialplanr)
library(oceandatr)

proj <- "ESRI:54009"

# Get eez to create grid
eez <- oceandatr::get_area("Micronesia", mregions_column = "sovereign1") %>%
  sf::st_transform(crs = proj) %>%
  sf::st_geometry() %>%
  sf::st_sf()

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


PUs <- spatialgridr::get_grid(area_polygon = eez,
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
  oceandatr::get_bathymetry(spatial_grid = PUs, keep = FALSE) %>% sf::st_drop_geometry(),
  oceandatr::get_geomorphology(spatial_grid = PUs) %>% sf::st_drop_geometry(),
  oceandatr::get_knolls(spatial_grid = PUs) %>% sf::st_drop_geometry(),
  oceandatr::get_seamounts_buffered(spatial_grid = PUs, buffer = 30000) %>% sf::st_drop_geometry(),
  oceandatr::get_coral_habitat(spatial_grid = PUs) %>% sf::st_drop_geometry(),
  oceandatr::get_enviro_regions(spatial_grid = PUs, max_num_clusters = 5)
) %>%
  dplyr::mutate(across(everything(), ~replace_na(.x, 0))) %>%  # Replace NA/NaN with 0
  dplyr::rename(geometry = x) # Temp fix while oceandatr return x



# Add cost data -----------------------------------------------------------

source("data-raw/get_gfwData.R")
gfw_cost <- get_gfwData("Micronesia", "2013-01-01", "2023-12-31", "yearly", "low", compress = TRUE) %>%
  dplyr::mutate(`Apparent Fishing Hours` = if_else(`Apparent Fishing Hours` > 1000, NA, `Apparent Fishing Hours`)) %>%
  sf::st_transform(sf::st_crs(PUs)) %>%
  sf::st_interpolate_aw(., PUs, extensive = TRUE, keep_NA = TRUE)



cost <- PUs %>%
  splnr_get_distCoast(custom_coast = coast) %>%  # Distance to nearest coast
  dplyr::rename(Cost_Distance = coastDistance_km) %>%
  dplyr::mutate(Cost_None = 0.1,
                Cost_Random = runif(dim(.)[1]),
                Cost_FishingHrs = tidyr::replace_na(gfw_cost$Apparent.Fishing.Hours, 0.00001)) %>%
  dplyr::relocate(geometry, .after = tidyselect::last_col())





# Locked in areas ---------------------------------------------------------

# TODO These are only point MPAs. Need some polygons to do this right.

lock_in <- "Micronesia (Federated States of)" %>%
  lapply(wdpar::wdpa_fetch,
         wait = TRUE,
         download_dir = rappdirs::user_data_dir("wdpar")
  ) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(.data$MARINE > 0) %>%
  sf::st_transform(crs = proj) %>%
  dplyr::select(geometry) %>%
  spatialgridr::get_data_in_grid(spatial_grid = PUs, dat = ., name = "MPAs")

ggplot(lock_in, aes(fill = MPAs)) + geom_sf()


# Save raw data -----------------------------------------------------------

dat_sf <- bind_cols(dat_sf,
                    lock_in %>% sf::st_drop_geometry(),
                    cost %>% sf::st_drop_geometry()) %>%
  dplyr::relocate(geometry, .after = tidyselect::everything())

rm(cost, lock_in, eez)


readr::write_rds(bndry, file = file.path("data-raw", "FSM", "FSM_Bndry.rds"))
readr::write_rds(coast, file = file.path("data-raw", "FSM", "FSM_Coast.rds"))
readr::write_rds(dat_sf, file = file.path("data-raw", "FSM", "FSM_Dat.rds"))


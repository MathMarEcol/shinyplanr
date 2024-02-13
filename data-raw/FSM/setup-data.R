library(tidyverse)
library(spatialplanr)
library(oceandatr)

## Check EEZ methods
# eez1 <- mregions2::mrp_get("eez", cql_filter = "sovereign1 = 'Australia'")
# eez1a <- eez1[5,]
# eez2 <- oceandatr::get_area("Australia")
# eez3 <- oceandatr::get_area("United States")

proj <- "ESRI:54009"

# Get eez to create grid
eez <- oceandatr::get_area("Micronesia", mregions_column = "sovereign1") %>%
  sf::st_transform(crs = proj) %>%
  sf::st_geometry() %>%
  sf::st_sf()

# TODO is there a better way to do the code below? For the app we need a boundary and the coastline.
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

source("data-raw/FSM/get_gfwData.R")
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
  oceandatr::data_to_planning_grid(planning_grid = PUs, dat = ., name = "MPAs") %>%
  dplyr::mutate(MPAs = forcats::as_factor(MPAs))

ggplot(lock_in, aes(fill = MPAs)) + geom_sf()


# Save raw data -----------------------------------------------------------

dat_sf <- bind_cols(dat_sf,
                    lock_in %>% sf::st_drop_geometry(),
                    cost %>% sf::st_drop_geometry()) %>%
  dplyr::relocate(geometry, .after = tidyselect::everything())

rm(cost, lock_in, eez)



save(dat_sf, bndry, coast, file = file.path("data-raw", "FSM", "FSM_TestData.rda"))


library(tidyverse)
library(spatialplanr)
library(oceandatr)
library(sf)
library(terra)

# TODO Add check that all names in Dict are unique. Or change target plotting to
# use variable name rather than the common name. It would be good to have the
# same Common name if needed.

data_path <- file.path("data-raw", "Kosrae", "KosraeData")

name <- "Kosrae"

#kosrae equal area projection from projection wizard, found using bbox of 12nm limits
kos_crs <- "+proj=cea +lon_0=163 +lat_ts=2.8 +datum=WGS84 +units=m +no_defs"

coast <-  sf::st_read(file.path(data_path, "kos_osm_shoreline.gpkg")) %>%
  sf::st_transform(crs = kos_crs)

bndry <- sf::st_read(file.path(data_path, "contour_500.gpkg")) %>%
  sf::st_transform(crs = kos_crs)

contour_500 <- read_sf(file.path(data_path, "contour_500.gpkg"))

pgrid <- get_grid(boundary = contour_500,
                  # resolution = 50,
                  resolution = 100,
                  crs = kos_crs,
                  touches = TRUE)

habitat_coral_seagrass <- read_sf(file.path(data_path, "benthic_geomorph_allen_intersect.gpkg")) %>%
  get_data_in_grid(spatial_grid = pgrid,
                   dat = .,
                   meth = "average",
                   feature_names = "habitat",
                   antimeridian = FALSE,
                   cutoff = 0.1)

habitat_other_reef <- read_sf(file.path(data_path, "benthic_no_coral_seagrass.gpkg")) %>%
  get_data_in_grid(spatial_grid = pgrid,
                   dat = .,
                   meth = "average",
                   feature_names = "Benthic_cover",
                   antimeridian = FALSE,
                   cutoff = 0.1)

depth_zones <- oceandatr::get_bathymetry(spatial_grid = pgrid,
                                         classify_bathymetry = TRUE,
                                         above_sea_level_isNA = FALSE,
                                         bathymetry_data_filepath = file.path(data_path, "gebco_2024_n6.0_s4.9_w162.0_e164.0.tif")) %>%
  mask(sum(c(habitat_coral_seagrass, habitat_other_reef), na.rm = TRUE) %>% subst(1:10, NA))

ous_fisheries_sectors <- list.files(file.path(data_path, "ocean-use-survey", "subsectors", "fishing"), pattern = "\\.tif$", full.names = TRUE) %>%
  lapply(rast) %>%
  lapply(FUN = function(x) get_data_in_grid(spatial_grid = pgrid, dat = x, meth = "average", antimeridian = FALSE)) %>%
  rast() %>%
  round() #avoid problems with fractional differences in OUS values driving prioritization results

ous_trochus_farm <- rast(file.path(data_path, "ocean-use-survey", "subsectors", "aquaculture", "trochus_farming.tif")) %>%
  get_data_in_grid(spatial_grid = pgrid, dat = ., meth = "average", antimeridian = FALSE)


# habitat_coral_seagrass <- st_read(file.path(data_path, "benthic_geomorph_allen_intersect.gpkg")) %>%
#   get_data_in_grid(spatial_grid = PUs,
#                    dat = .,
#                    meth = "average",
#                    feature_names = "habitat",
#                    antimeridian = FALSE,
#                    cutoff = 0.1)
#
# habitat_other_reef <- st_read(file.path(data_path, "benthic_no_coral_seagrass.gpkg")) %>%
#   get_data_in_grid(spatial_grid = PUs,
#                    dat = .,
#                    meth = "average",
#                    feature_names = "Benthic_cover",
#                    antimeridian = FALSE,
#                    cutoff = 0.1)
#
# depth_zones <- oceandatr::get_bathymetry(
#   spatial_grid = PUs,
#   classify_bathymetry = TRUE,
#   above_sea_level_isNA = FALSE,
#   bathymetry_data_filepath = file.path(data_path, "gebco_2024_n6.0_s4.9_w162.0_e164.0.tif")) # %>%
#   # mask(sum(c(habitat_coral_seagrass, habitat_other_reef), na.rm = TRUE) %>% subst(1:10, NA))
#
# convert_rast <- function(r, PUs){
#
#   out <- terra::rast(r) %>%
#     terra::round() %>% #avoid problems with fractional differences in OUS values driving prioritization results
#     terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm = TRUE, na.all = TRUE, round = FALSE) %>%
#     sf::st_as_sf() %>%
#     get_data_in_grid(spatial_grid = PUs, dat = ., meth = "average", antimeridian = FALSE)
#
# }
#
# ous_fisheries_sectors <- list.files(file.path(data_path, "ocean-use-survey", "subsectors", "fishing"),
#                                     pattern = "\\.tif$",
#                                     full.names = TRUE) %>%
#   purrr::map(convert_rast, PUs)
#
# ous_fisheries_sectors <- ous_fisheries_sectors %>%
#   purrr::map(sf::st_drop_geometry) %>%
#   bind_cols(PUs) %>%
#   sf::st_as_sf()
#
#
# ous_trochus_farm <- terra::rast(file.path(data_path, "ocean-use-survey", "subsectors", "aquaculture", "trochus_farming.tif")) %>%
#   terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm = TRUE, na.all = TRUE, round = FALSE) %>%
#   sf::st_as_sf() %>%
#   get_data_in_grid(spatial_grid = PUs,
#                    dat = .,
#                    meth = "average",
#                    antimeridian = FALSE)
#
# dat_sf <- bind_cols(habitat_coral_seagrass %>% sf::st_drop_geometry(),
#                     habitat_other_reef %>% sf::st_drop_geometry(),
#                     ous_fisheries_sectors %>% sf::st_drop_geometry(),
#                     ous_trochus_farm %>% sf::st_drop_geometry(),
#                     depth_zones) %>%
#   sf::st_as_sf()


dat <- c(habitat_coral_seagrass, habitat_other_reef, ous_fisheries_sectors, ous_trochus_farm, depth_zones)

dat_sf <- dat %>%
  terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm = TRUE, na.all = TRUE, round = FALSE) %>%
  sf::st_as_sf() %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  janitor::clean_names()

PUs <- dat_sf %>%
  dplyr::select(geometry)

# Add cost data -----------------------------------------------------------

gfw_cost <- spatialplanr::splnr_get_gfw("Micronesia",
                                        start_date = "2013-01-01",
                                        end_date = "2023-12-31",
                                        temp_res = "YEARLY",
                                        spat_res = "LOW",
                                        compress = TRUE) %>%
  dplyr::select(-GFWregionID) %>%
  dplyr::mutate(ApparentFishingHrs = if_else(ApparentFishingHrs > 1000, NA, ApparentFishingHrs)) %>%
  sf::st_transform(sf::st_crs(PUs)) %>%
  sf::st_interpolate_aw(., PUs, extensive = TRUE, keep_NA = TRUE)

ous_fish <- terra::rast(file.path(data_path, "ocean-use-survey", "fisheries.tif")) %>%
  get_data_in_grid(spatial_grid = pgrid, dat = ., meth = "average", antimeridian = FALSE) %>%
  terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm = TRUE, na.all = TRUE, round = FALSE) %>%
  sf::st_as_sf() %>%
  sf::st_transform(kos_crs) %>%
  sf::st_interpolate_aw(PUs, extensive = FALSE, na.rm = TRUE, keep_NA = TRUE)


cost <- PUs %>%
  splnr_get_distCoast(custom_coast = coast) %>%  # Distance to nearest coast
  dplyr::rename(Cost_Distance = coastDistance_km) %>%
  dplyr::mutate(Cost_OUS = ous_fish$fisheries,
                Cost_None = 0.1,
                Cost_Random = runif(dim(.)[1]),
                Cost_FishingHrs = tidyr::replace_na(gfw_cost$ApparentFishingHrs, 0.00001)) %>%
  dplyr::relocate(geometry, .after = tidyselect::last_col())



# Locked in areas ---------------------------------------------------------

# TODO These are only point MPAs. Need some polygons to do this right.

# lock_in <- "Micronesia (Federated States of)" %>%
#   lapply(wdpar::wdpa_fetch,
#          wait = TRUE,
#          download_dir = rappdirs::user_data_dir("wdpar")
#   ) %>%
#   dplyr::bind_rows() %>%
#   dplyr::filter(.data$MARINE > 0) %>%
#   sf::st_transform(crs = kos_crs) %>%
#   dplyr::select(geometry) %>%
#   spatialgridr::get_data_in_grid(spatial_grid = PUs, dat = ., name = "MPAs")
#
# ggplot(lock_in, aes(fill = MPAs)) + geom_sf()


# Climate Data ------------------------------------------------------------

# Start with 1 climate layer called metric. Then come back and add other layers wth unique names
climate_sf <- bind_cols(
  read_rds(file.path("data-raw", "Kosrae", "climate_data", "tos_Omon_trends_ssp245_r1i1p1f1_RegriddedAnnual_20150101-21001231.rds")) %>%
    sf::st_drop_geometry() %>% dplyr::select("slpTrends_245"="slpTrends"),
  read_rds(file.path("data-raw", "Kosrae", "climate_data", "tos_Omon_trends_ssp370_r1i1p1f1_RegriddedAnnual_20150101-21001231.rds")) %>%
    sf::st_drop_geometry() %>% dplyr::select("slpTrends_370"="slpTrends"),
  read_rds(file.path("data-raw", "Kosrae", "climate_data", "tos_Omon_trends_ssp585_r1i1p1f1_RegriddedAnnual_20150101-21001231.rds")) %>%
    dplyr::select("slpTrends_585"="slpTrends")) %>%
  sf::st_as_sf() %>%
  sf::st_transform(kos_crs) %>%
  sf::st_interpolate_aw(dat_sf, extensive = FALSE, na.rm = TRUE, keep_NA = TRUE)


# Save raw data -----------------------------------------------------------

dat_sf <- bind_cols(dat_sf,
                    # lock_in %>% sf::st_drop_geometry(),
                    cost %>% sf::st_drop_geometry(),
                    climate_sf %>% sf::st_drop_geometry()) %>%
  dplyr::relocate(geometry, .after = tidyselect::everything())

save(dat_sf, bndry, coast, climate_sf, file = file.path("data-raw", name, paste0(name,"_RawData.rda")))


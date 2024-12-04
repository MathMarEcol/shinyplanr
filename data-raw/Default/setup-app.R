## Code to prepare the app goes here

library(tidyverse)

country <- ""
data_dir <- file.path("data-raw", country)

# TODO Write function to load default options
# Especially different colours for the plotting etc

# APP PARAMETERS --------------------------------------------------
options <- list(

  ## General Options
  nav_title = "ADD TITLE", # Navbar title
  nav_primary = "#2C3E50", # Hex colour codes: https://htmlcolorcodes.com
  # "#85929E"
  # "#F8C471"
  # "#73C6B6"
  # "#85C1E9"
  # "#E74C3C"
  # "#2C3E50"

  ## File locations
  file_logo = file.path(data_dir, "logos", "WaittSquareLogo_invert.png"),
  file_data = file.path(data_dir, paste0(country, "_RawData.rda")),
  file_climate = file.path(data_dir, "climate_data", "tos_ensemble_ssp245.rds"),

  ## App Setup Options
  mod_1welcome = TRUE, #switch modules on/off
  mod_2scenario = TRUE, #switch modules on/off
  mod_3compare = TRUE, #switch modules on/off
  mod_4features = TRUE, #switch modules on/off
  mod_6help = TRUE, #switch modules on/off
  mod_7credit = TRUE, #switch modules on/off

  # TODO Get this working - get conditional panels working for global variables

  climate_change = 1, #switch climate change on/off; 0 = not clim-smart; 1 = CPA; 2 = Feature; 3 = Percentile
  # Warning: still requires some changes in the app: direction, percentile etc. should this be in here? those are input options to the functions

  lockedInArea = 1, #Includes locked in areas

  ## Which objective function module are we using
  obj_func = "min_set", # Minimum set objective
  # obj_func = min_shortfall # Minimum shortfall objective

  ## Geographic Options
  cCRS = "ESRI:54009"

)


#append list with climate-smart values
if (options$climate_change == 1) {

  options$refugiaTarget = 1 # default: 1
  options$percentile = 5 # default: 5
  options$direction = 1 # depend on where more climate-smart areas are. 1: clim-smart at high numbers; -1: clim-smart in lower values

} else if (options$climate_change == 2) {

  options$refugiaTarget = 0.3 # default: 0.3
  options$percentile = 35 # default: 35
  options$direction = 1 # depend on where more climate-smart areas are. 1: clim-smart at high numbers; -1: clim-smart in lower values

} else if (options$climate_change == 3) {

  options$refugiaTarget = NA # not needed for percentile
  options$percentile = 35 # default: 35
  options$direction = 1 # depend on where more climate-smart areas are. 1: clim-smart at high numbers; -1: clim-smart in lower values

}



# Copy logo to required directory
file.copy(options$file_logo, file.path("inst", "app", "www", "logo.png"), overwrite = TRUE)


# DATASETS --------------------------------------------------------

# A dictionary of all data and feature-specific set up values
Dict <- readr::read_csv(file.path(data_dir, "Dict_Feature.csv")) %>%
  dplyr::filter(includeApp) %>% # Only those features to be included
  dplyr::arrange(.data$type, .data$category, .data$nameCommon)

vars <- Dict %>%
  dplyr::filter(!type == "Justification") %>%
  dplyr::pull(nameVariable)


# An sf object for all layers
load(options$file_data)

raw_sf <- dat_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::select(tidyselect::all_of(vars)) %>%
  dplyr::select(which(!colSums(., na.rm=TRUE) %in% 0)) %>% # Remove all zero columns
  dplyr::bind_cols(dat_sf %>% dplyr::select(geometry)) # Add geometry back in

# Check if variables were removed from the data due to zero columns
if (length(vars) != dim(raw_sf)[2]-1){
  stop("raw_sf and the Dictionary have different numbers of variables. If columns
       were removed due to being all zero above, please remove corresponding
       variable from Dict")}


# # Include the climate data if needed.
climate_sf <- readRDS(options$file_climate) %>%
  dplyr::select(-velocityRescaled, -exposureRescaled) %>%
  dplyr::rename(metric = gMean)
climate_sf <- NULL

# Plotting Overlays -------------------------------------------------------

bndry <- bndry

overlay <- coast

# TODO Work out how to add options here without having to define all.
# Change to a list called plot_options()? that is passed to the function.


# MODULE 1 - WELCOME ------------------------------------------------------
tx_1welcome <- readr::read_file(file.path(data_dir, "html_1welcome.txt"))

# MODULE 2 - SCENARIO ------------------------------------------------------

# MODULE 3 - COMPARISON ------------------------------------------------------

# MODULE 6 - HELP ------------------------------------------------------
tx_6faq <- readr::read_file(file.path(data_dir, "html_6faq.txt"))
tx_6changelog <- readr::read_file(file.path(data_dir, "html_6changelog.txt"))
tx_6technical <- readr::read_file(file.path(data_dir, "html_6technical.txt"))
tx_6references <- readr::read_file(file.path(data_dir, "html_6references.txt"))

# MODULE 7 - CREDIT ------------------------------------------------------
tx_7credit <- readr::read_file(file.path(data_dir, "html_7credit.txt"))

# HEX STICKER -------------------------------------------------------------
# Create app-specific Hex sticker if wanted. Otherwise the generic shinyplanr one will be used
hexSticker::sticker(options$file_logo,
                    package="",
                    s_x=1,
                    s_y=1,
                    s_width=.8,
                    h_fill = "#0033a0",
                    h_color = "grey40",
                    # url = "",
                    # u_color = "white",
                    dpi=600,
                    filename=file.path("inst", "app", "www", "Hex.png"))

golem::use_favicon(file.path("inst", "app", "www", "Hex.png"), pkg = golem::get_golem_wd(), method = "curl")


# PLOTTING THEME -----------------------------------------------------------
map_theme <- list(
  ggplot2::theme_bw(),
  ggplot2::theme(
    legend.position = "right",
    legend.direction = "vertical",
    # text = ggplot2::element_text(size = 6, colour = "black"),
    axis.text = ggplot2::element_text(size = 9, colour = "black"),
    plot.title = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_text(size = 9),
    legend.text = ggplot2::element_text(size = 9),
    axis.title = ggplot2::element_blank()
  )
)

bar_theme <- list(
  ggplot2::theme_bw(),
  ggplot2::theme(
    legend.position = "right",
    legend.direction = "vertical",
    # text = ggplot2::element_text(size = 6, colour = "black"),
    axis.text = ggplot2::element_text(size = 6, colour = "black"),
    plot.title = ggplot2::element_text(size = 12),
    legend.title = ggplot2::element_text(size = 9),
    legend.text = ggplot2::element_text(size = 9),
    axis.title = ggplot2::element_blank()
  )
)

usethis::use_data(options,
                  map_theme,
                  bar_theme,
                  Dict,
                  vars,
                  raw_sf,
                  climate_sf,
                  bndry,
                  overlay,
                  tx_1welcome,
                  tx_6faq,
                  tx_6technical,
                  tx_6changelog,
                  tx_6references,
                  tx_7credit,
                  overwrite = TRUE,
                  internal = TRUE)

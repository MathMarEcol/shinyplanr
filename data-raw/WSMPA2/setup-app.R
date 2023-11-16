## code to prepare the app goes here

library(tidyverse)


# APP PARAMETERS --------------------------------------------------
options <- list(

  ## General Options
  nav_title = "shinyDANr", # Navbar title

  ## File locations
  file_logo = file.path("data-raw", "WSMPA2", "logos", "NP-logo-hvit_engelsk.png"),
  file_data = file.path("data-raw", "WSMPA2", "WSMPAII_features.rds"),
  file_climate = file.path("data-raw", "WSMPA2", "Ensemble-ssp585-combined.rds"),

  ## App Setup Options
  mod_1welcome = TRUE, #switch modules on/off
  mod_2scenario = TRUE, #switch modules on/off
  mod_3compare = FALSE, #switch modules on/off
  mod_4features = TRUE, #switch modules on/off
  mod_6help = TRUE, #switch modules on/off
  mod_7credit = TRUE, #switch modules on/off

  # TODO Get this working - get conditional panels working for global variables
  #also make conditional panel for mainbody for plots?
 climate_change = 1, #switch climate change on/off; 0 = not clim-smart; 1 = CPA; 2 = Feature; 3 = Percentile
#Warning: still requires some changes in the app: direction, percentile etc. should this be in here? those are input options to the functions


  # obj_func = # which objective function module are we using

  ## Geographic Options
  cCRS = "+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs", #SthPoleEqualArea
  Limits = c(xmin = 0, xmax = 30, ymin = -70.5, ymax = -60),
  Shape = "Hexagon", # Shape of PUs
  PU_size = 100 # km2
)


# Copy logo to required directory
file.copy(options$file_logo, file.path("inst", "app", "www", "logo.png"), overwrite = TRUE)


# DATASETS --------------------------------------------------------

# A dictionary of all data and feature-specific set up values
Dict <- readr::read_csv(file.path("data-raw","WSMPA2", "Dict_Feature.csv")) %>%
  dplyr::filter(IncludeApp) %>% # Only those features to be included
  dplyr::arrange(.data$Type, .data$Category, .data$NameCommon)

vars <- Dict %>%
  dplyr::filter(!Type == "Justification") %>%
  dplyr::pull(NameVariable)

# TODO Consider using Dict for this and not creating a new dataframe
# It would just mean cleaning up the names a little
category <- Dict %>%
  dplyr::filter(!Type %in% c("Cost", "Justification")) %>%
  dplyr::select(NameVariable, Category) %>%
  dplyr::rename("feature" = NameVariable, "category" = "Category") # TODO Consider adapting the spatialplanr code to remove the need for this

# An sf object for all layers
raw_sf <- readRDS(options$file_data)

raw_sf <- raw_sf %>%
  dplyr::mutate(Cost_None = 0.1) %>% # Add a column for no cost
  dplyr::select(tidyselect::all_of(vars)) %>%
  dplyr::select(which(!colSums(., na.rm=TRUE) %in% 0)) %>% # Remove all zero columns
  dplyr::bind_cols(raw_sf %>% dplyr::select(geometry)) # Add geometry back in

# Check if variables were removed from the data due to zero columns
if (length(vars) != dim(raw_sf)[2]-1){
  stop("raw_sf and the Dictionary have different numbers of variables. If columns
       were removed due to being all zero above, please remove corresponding
       variable from Dict")}


# Include the climate data if needed.
climate_sf <- readRDS(options$file_climate) %>%
  dplyr::select(-velocityRescaled, -exposureRescaled) %>%
  dplyr::rename(metric = gMean)

# Load Antarctic shapefile from Teschke et al 2020
landmass <- sf::st_read(file.path("data-raw", "WSMPA2", "Boundaries", "Antarctica.shp"), quiet = TRUE) %>%
  sf::st_transform(crs = options$cCRS) # Transform to SthPoleEqualArea

# # Load the Ice Shelf
# IceShelf <- sf::st_read(file.path("data-raw", "WSMPA2", "Boundaries","Ice_shelf.shp"), quiet = TRUE) %>%
#   sf::st_transform(crs = cCRS) # Transform to SthPoleEqualArea


# MODULE 1 - WELCOME ------------------------------------------------------
tx_1welcome <- readr::read_file(file.path("data-raw", "WSMPA2", "html_1welcome.txt"))

# return_list <- read_textboxes(FILENAME)



# MODULE 2 - SCENARIO ------------------------------------------------------



# MODULE 3 - COMPARISON ------------------------------------------------------



# MODULE 6 - HELP ------------------------------------------------------
tx_6faq <- readr::read_file(file.path("data-raw", "WSMPA2", "html_6faq.txt"))
tx_6changelog <- readr::read_file(file.path("data-raw", "WSMPA2", "html_6changelog.txt"))
tx_6technical <- readr::read_file(file.path("data-raw", "WSMPA2", "html_6technical.txt"))
tx_6references <- readr::read_file(file.path("data-raw", "WSMPA2", "html_6references.txt"))


# MODULE 7 - CREDIT ------------------------------------------------------
tx_7credit <- readr::read_file(file.path("data-raw", "WSMPA2", "html_7credit.txt"))



# HEX STICKER -------------------------------------------------------------
# Create app-specific Hex sticker if wanted. Otherwise the generic shinyplanr one will be used
hexSticker::sticker(options$file_logo,
                    package="",
                    s_x=1,
                    s_y=1.1,
                    s_width=.8,
                    h_fill = "black", #"#0033a0",
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
                  landmass,
                  Dict,
                  vars,
                  category,
                  raw_sf,
                  climate_sf,
                  tx_1welcome,
                  tx_6faq,
                  tx_6technical,
                  tx_6changelog,
                  tx_6references,
                  tx_7credit,
                  overwrite = TRUE,
                  internal = TRUE)

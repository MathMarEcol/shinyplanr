# Some code to create a hex sticker for the spatialplanr package
#
# Last updated: Saturday 8th July 2023
#
# Jason D. Everett (UQ/CSIRO/UNSW)
#
# For installation instructions see here:
# https://github.com/GuangchuangYu/hexSticker

# devtools::install_github("GuangchuangYu/hexSticker")

library(spatialplanr)

Shape <- "Hexagon" # "Shape of PUs
# PU_size <- 100000 # km2
# cCRS <- "ESRI:54009" # Mollweide
# cCRS <- "ESRI:54030" # Robinson
cCRS <- "EPSG:4326"

blx <- 5 # bottom left x
brx <- 145 # bottom right x

xi1 <- seq(75, brx, 1) # Top right
yi1 <- pracma::interp1(c(75, brx), c(80, 40), xi1, method = "linear")

yi2 <- seq(40, -30, -1) # Right side
xi2 <- pracma::interp1(c(40, -30), c(brx, brx), yi2, method = "linear")

xi3 <- seq(brx, 75, -1) # Bottom right
yi3 <- pracma::interp1(c(brx, 75), c(-30, -70), xi3, method = "linear")

xi4 <- seq(75, blx, -1) # Bottom left
yi4 <- pracma::interp1(c(75, blx), c(-70, -30), xi4, method = "linear")

yi5 <- seq(40, -30, -1) # Left side
xi5 <- pracma::interp1(c(40, -30), c(blx, blx), yi5, method = "linear")

xi6 <- seq(blx, 75) # Top left
yi6 <- pracma::interp1(c(blx, 75), c(40, 80), xi6, method = "linear")

Bndry <- dplyr::tibble(
  x = c(xi1, xi2, xi3, xi4, xi5, xi6),
  y = c(yi1, yi2, yi3, yi4, yi5, yi6)
) %>%
  as.matrix() %>%
  list() %>%
  sf::st_polygon() %>%
  sf::st_sfc(crs = "EPSG:4326") %>%
  sf::st_sf() %>%
  sf::st_make_valid()

ggplot2::ggplot(data = Bndry) +
  ggplot2::geom_sf()

landmass <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  sf::st_make_valid() %>%
  sf::st_intersection(Bndry) %>%
  sf::st_make_valid()

ggplot2::ggplot() +
  ggplot2::geom_sf(data = landmass) +
  ggplot2::geom_sf(data = Bndry, fill = NA)


PUs <- sf::st_make_grid(Bndry,
                        square = FALSE,
                        cellsize = c(2, 2),
                        what = "polygons"
) %>%
  sf::st_sf() %>%
  sf::st_intersection(Bndry) %>%
  sf::st_make_valid()

PUs <- PUs %>%
  dplyr::mutate(Prob = runif(dim(PUs)[1]))



# read the jpef file from device
img <- grid::rasterGrob(png::readPNG(file.path("data-raw", "shiny.png"), native = TRUE))



# Possible colours...
# https://www.pinterest.com.au/pin/sunset-color-scheme-image-search-results-in-2023--224194887692504381/

(gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = PUs, colour = "#fdf7c2", linewidth = 0.05, show.legend = FALSE, ggplot2::aes(fill = Prob)) +
    ggplot2::scale_fill_gradient(
      low = "#9ac4e1", # "#84dfd3"
      high = "#3b81bd"
    ) +
    ggplot2::geom_sf(data = landmass, colour = "grey40", fill = "grey20", alpha = 1, linewidth = 0.05, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(PUs)$xlim, ylim = sf::st_bbox(PUs)$ylim) +
    ggplot2::theme_void() +
    ggplot2::annotation_custom(img, xmin = 5, xmax = 85, ymin = -25, ymax = 25)
)



hexSticker::sticker(gg,
                    package = "planr",
                    p_y = 1.1,
                    p_x = 1.45,
                    p_color = "white",
                    p_size = 85,
                    p_fontface = "bold",
                    s_x = 1,
                    s_y = 1,
                    s_width = 2,
                    s_height = 4,
                    # h_fill = "#9FE2BF",
                    h_color = "black", # "grey40",
                    dpi = 1000,
                    asp = 1,
                    filename = file.path("data-raw", "shinyplanr.png")
)

# Create favicons for the site
pkgdown::build_favicons(pkg = ".", overwrite = FALSE)


# Original

#
# (gg <- ggplot2::ggplot() +
#     ggplot2::geom_sf(data = PUs, colour = "black", linewidth = 0.1, show.legend = FALSE, ggplot2::aes(fill = Prob)) +
#     ggplot2::geom_sf(data = landmass, colour = "grey30", fill = "grey50", alpha = 1, linewidth = 0.1, show.legend = FALSE) +
#     ggplot2::coord_sf(xlim = sf::st_bbox(PUs)$xlim, ylim = sf::st_bbox(PUs)$ylim) +
#     ggplot2::theme_void())
#
#
# hexSticker::sticker(gg,
#                     package = "spatialplanr",
#                     p_y = 1,
#                     p_x = 1,
#                     p_color = "grey90",
#                     p_size = 80,
#                     p_fontface = "bold",
#                     s_x = 1,
#                     s_y = 1,
#                     s_width = 2,
#                     s_height = 4,
#                     # h_fill = "#9FE2BF",
#                     h_color = "grey20",
#                     dpi = 1000,
#                     asp = 1,
#                     filename = file.path("man", "figures", "logo.png"))

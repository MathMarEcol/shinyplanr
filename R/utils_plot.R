# Regionalisation Plot ----------------------------------------------------
#'
#' @noRd
#'
create_regionPlot <- function(df) {
  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = df, ggplot2::aes(fill = .data$region, color = .data$region)) +
    ggplot2::scale_color_viridis_d(
      name = "Regionalization",
      aesthetics = c("colour", "fill"),
      guide = ggplot2::guide_legend(
        override.aes = list(linetype = 0),
        title.position = "top",
        ncol = 1,
        order = 1,
        keyheight = grid::unit(0.02, "npc"),
        keywidth = grid::unit(0.02, "npc")
      )
    )
  return(gg)
}


# Cost Overlay Plot -------------------------------------------------------
#'
#' @noRd
#'
create_costPlot <- function(cost_sf, titleCost) {
  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = cost_sf, ggplot2::aes(fill = .data$Cost), alpha = 0.8, colour = NA, size = 0.0001) +
    ggplot2::scale_fill_distiller(
      name = titleCost,
      palette = "Oranges",
      limits = c(
        stats::quantile(cost_sf$Cost, 0.01),
        stats::quantile(cost_sf$Cost, 0.99)
      ),
      direction = 1,
      oob = scales::squish,
      guide = ggplot2::guide_colourbar(
        title.position = "bottom",
        title.hjust = 0.5,
        order = 1,
        barheight = grid::unit(0.03, "npc"),
        barwidth = grid::unit(0.25, "npc")
      )
    )
  return(gg)
}


# Selection Frequency Plot ------------------------------------------------
#' Selection frequency Plot --> requires a problem formulated with a portfolio
#' @noRd
#'
#'
create_selFreqPlot <- function(df) {
  colourCount <- length(unique(df$selectionFreq))

  gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = df, ggplot2::aes(fill = .data$selectionFreq), colour = NA) +
    ggplot2::scale_fill_manual(
      values = RColorBrewer::brewer.pal(colourCount, "Blues"),
      name = "Selection Frequency",
      guide = ggplot2::guide_legend(
        override.aes = list(linetype = 0),
        title.position = "right",
        order = 1,
        keyheight = grid::unit(0.05, "npc"),
        keywidth = grid::unit(0.05, "npc")
      )
    )

  return(gg)
}


# Climate Feature Plot ----------------------------------------------------
#' Climate Feature Plot
#'
#' @noRd
#'
create_climDataPlot <- function(df) {
  gg_clim <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = df %>% sf::st_as_sf(), ggplot2::aes(fill = .data$metric), colour = NA) +
    ggplot2::scale_fill_viridis_c(
      name = "Climate resilience metric\n(climate exposure and velocity)",
      option = "C",
      guide = ggplot2::guide_colourbar(
        title.position = "bottom",
        title.hjust = 0.5,
        order = 1,
        barheight = grid::unit(0.03, "npc"),
        barwidth = grid::unit(0.25, "npc")
      )
    )

  return(gg_clim)
}


# Climate Kernel Density Plot ---------------------------------------------
#' Climate kernel density plot
#'
#' @noRd
#'
create_climKernelDensityPlot <- function(soln) {
  soln$approach <- "Ridge" # Need a dummy variable here.

  ggRidge <- ggplot2::ggplot() +
    ggridges::stat_density_ridges(
      data = soln %>% dplyr::filter(.data$solution_1 == 1) %>% dplyr::mutate(solution_1 = "Selected"),
      ggplot2::aes(x = .data$metric, y = .data$approach, fill = .data$solution_1),
      # fill = "#3182bd",
      color = "#194361", quantile_lines = TRUE, quantiles = 2,
      show.legend = TRUE
    ) +
    ggridges::stat_density_ridges(
      data = soln %>% dplyr::filter(.data$solution_1 == 0) %>% dplyr::mutate(solution_1 = "Not Selected"),
      ggplot2::aes(x = .data$metric, y = .data$approach, fill = .data$solution_1),
      # fill = "#c6dbef",
      color = "#3182bd", quantile_lines = TRUE, quantiles = 2,
      alpha = 0.5,
      show.legend = TRUE
    ) +
    ggplot2::scale_x_continuous(
      name = "Climate resilience metric",
      breaks = c(min(soln$metric), max(soln$metric)),
      labels = c("less climate-resilient", "more climate-resilient")
    ) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::labs(
      x = "Climate resilience metric",
      y = "Proportion of planning units"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.ticks = ggplot2::element_line(color = "black", size = 1),
      text = ggplot2::element_text(size = 20),
      axis.line = ggplot2::element_line(colour = "black", size = 1),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 20),
      axis.title = ggplot2::element_text(size = 20),
      legend.title = ggplot2::element_text(color = "black", angle = 270, hjust = 0.5),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 20)
    ) +
    ggplot2::scale_fill_manual(
      name = "",
      values = c("Not Selected" = "#c6dbef", "Selected" = "#3182bd"),
      aesthetics = "fill",
      guide = ggplot2::guide_legend(
        override.aes = list(linetype = 0),
        nrow = 1
      )
    )

  return(ggRidge)
}




#' Get solution text for plot
#'
#' @noRd
fSolnText <- function(input, sDat, col_name = "solution_1") {
  sDat <- sDat %>%
    sf::st_drop_geometry() %>%
    dplyr::select(input$costid, !!rlang::sym(col_name))

  totalCost <- sDat %>%
    dplyr::select(-!!rlang::sym(col_name)) %>%
    sum()

  outsideCost <- sDat %>%
    dplyr::filter(!!rlang::sym(col_name) == 0) %>%
    dplyr::select(-!!rlang::sym(col_name)) %>%
    sum()

  PU_count <- sDat %>%
    dplyr::filter(!!rlang::sym(col_name) == 1) %>%
    nrow()

  txt_soln <- paste0(round(PU_count / nrow(sDat) * 100), "% of planning region selected in MPAs")
  txt_cost <- paste0(round((outsideCost / totalCost) * 100), "% of total rational use values outside MPAs")

  out <- list(txt_soln, txt_cost)
  return(out)
}



#' Calculate target data for plotting and display
#'
#' @noRd
#'
ftargetPlotData <- function(sDat, pDat, targets, climsmart = FALSE, soln = "solution_1", Dict = Dict) {
  s1 <- sDat %>%
    dplyr::mutate(solution = dplyr::if_else(!!rlang::sym(soln) == "Selected", 1, 0)) %>% # Add binary (1,0) values
    tibble::as_tibble()

  df <- prioritizr::eval_feature_representation_summary(pDat, s1[, "solution"])

  if (climsmart == TRUE) {
    # STILL TO DO
    df <- df %>%
      dplyr::select(-.data$relative_held) %>%
      dplyr::mutate(
        feature = stringr::str_remove_all(.data$feature, "_CS"),
        feature = stringr::str_remove_all(.data$feature, "_NCS")
      ) %>% # Ensure all features have the same name.
      dplyr::group_by(.data$feature) %>%
      dplyr::summarise(
        total_amount = sum(.data$total_amount), # Sum the features together
        absolute_held = sum(.data$absolute_held)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(relative_held = .data$absolute_held / .data$total_amount) %>% # Calculate proportion
      dplyr::select(-.data$total_amount, -.data$absolute_held) # Remove extra columns
  }


  df <- df %>%
    dplyr::mutate(relative_held = .data$relative_held * 100) %>% # Change to percentage
    dplyr::left_join(Dict %>% dplyr::select("NameCommon", "NameVariable", "Category"), by = c("feature" = "NameVariable")) %>%
    dplyr::left_join(targets, by = c("feature" = "features")) %>%
    dplyr::select(-"feature") %>%
    stats::na.omit() %>%
    dplyr::rename(
      value = .data$relative_held,
      feature = .data$NameCommon
    ) %>%
    dplyr::mutate(incidental = ifelse(targets > 0, FALSE, TRUE)) # Is the protection incidental


  return(df)
}


# Binary Map --------------------------------------------------------------
#' Binary Map
#'
#' @noRd
#'
create_binPlot <- function(df, colInterest, values = c("Selected" = "#3182bd", "Not Selected" = "#c6dbef"), title = "") {
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = df, ggplot2::aes(fill = colInterest), colour = NA, size = 0.001) +
    ggplot2::scale_fill_manual(
      name = title,
      values = values,
      aesthetics = "fill",
      guide = ggplot2::guide_legend(
        override.aes = list(linetype = 0),
        nrow = 2,
        order = 1,
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
    )
  return(p)
}

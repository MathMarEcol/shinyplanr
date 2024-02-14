
#' Return category df from Dict
#'
#' @noRd
#'
fget_category <- function(Dict){

  category <- Dict %>%
    dplyr::filter(!type %in% c("Cost", "Justification")) %>%
    dplyr::select(nameVariable, category) %>%
    dplyr::rename(feature = nameVariable)
  # TODO I want to remove this last command and have the app deal with `nanmeVariable`

  return(category)
}



# Get Targets

#' Calculate targets based on inputs
#'
#' @noRd
#'
fget_targets<- function(input, name_check = "sli_"){

  ft <- Dict %>%#vars[stringr::str_detect(vars, "Cost_", negate = TRUE)] %>%
    dplyr::filter(type != "Constraint", type != "Cost") %>%
    dplyr::pull(nameVariable)

  targets <- ft %>%
    purrr::map(\(x) rlang::eval_tidy(rlang::parse_expr(paste0("input$", paste0(name_check, x))))) %>%
    tibble::enframe() %>%
    tidyr::unnest(cols = .data$value) %>%
    dplyr::rename(feature = "name", target = "value") %>%
    dplyr::mutate(feature = ft) %>%
    dplyr::mutate(target = target / 100) # requires number between 0-1

  return(targets)
}


#' Define conservation problem
#'
#' @noRd
#'
fdefine_problem <- function(targets, input, name_check = "sli_", clim_input = FALSE, cost_var = input$costid){

  # TODO raw_sf is not passed into the function

  out_sf <- raw_sf %>%
    dplyr::select(
      .data$geometry,
      tidyselect::all_of(targets$feature),
      tidyselect::starts_with("Cost_")
    ) %>%
    sf::st_as_sf()

  if (clim_input == FALSE) {
    p_dat <- out_sf
  } else if (clim_input == TRUE) {
    featuresDF <- out_sf %>%
      dplyr::select(
        .data$geometry,
        tidyselect::all_of(targets$feature)
      ) %>%
      dplyr::mutate(cellID = 1:nrow(.))

    if (options$climate_change == 1) { # CPA approach

      CS_Approach <- spatialplanr::splnr_climate_priorityAreaApproach(
        featuresDF = featuresDF, percentile = options$percentile,
        metricDF = climate_sf, targetsDF = targets, direction = options$direction, refugiaTarget = options$refugiaTarget
      )
    } else if (options$climate_change == 2) { # feature approach

      CS_Approach <- spatialplanr::splnr_climate_featureApproach(
        featuresDF = featuresDF, percentile = options$percentile,
        metricDF = climate_sf, targetsDF = targets, direction = options$direction, refugiaTarget = options$refugiaTarget
      )
    } else if (options$climate_change == 3) { # percentile approach

      CS_Approach <- spatialplanr::splnr_climate_percentileApproach(
        featuresDF = featuresDF, percentile = options$percentile,
        metricDF = climate_sf, targetsDF = targets, direction = options$direction
      )
    }

    targets <- CS_Approach$Targets

    p_dat <- CS_Approach$Features %>%
      dplyr::left_join(
        out_sf %>%
          sf::st_drop_geometry() %>%
          dplyr::mutate(cellID = 1:nrow(.)) %>%
          dplyr::select(
            "cellID",
            tidyselect::starts_with("Cost_")
          ),
        by = "cellID"
      ) %>%
      dplyr::left_join(climate_sf %>%
                         sf::st_drop_geometry(), by = "cellID")
  } else {
    print("Something odd is going on here. Check climate-smart tick box.")
  }

  f_no <- fCheckFeatureNo(p_dat) # Check number of features

  if (f_no == 1) {
    shinyalert::shinyalert("Error", "No features have been selected. You can't run a spatial prioritization without any features.",
                           type = "error",
                           callbackR = shinyjs::runjs("window.scrollTo(0, 0)")
    )

    p_dat <- p_dat %>%
      dplyr::mutate(DummyVar = 1)

    p1 <- prioritizr::problem(p_dat, "DummyVar", cost_var) %>%
      prioritizr::add_min_set_objective() %>%
      prioritizr::add_relative_targets(0) %>%
      prioritizr::add_binary_decisions() %>%
      prioritizr::add_cbc_solver(verbose = TRUE)

  } else {
    ## Get names of the features
    if (clim_input == TRUE) {
      usedFeatures <- p_dat %>%
        sf::st_drop_geometry() %>%
        dplyr::select(-tidyselect::starts_with("Cost_"), -.data$cellID, -.data$metric) %>%
        names()
    } else {
      usedFeatures <- targets$feature
    }

    if (options$obj_func == "min_set") {
      p1 <- prioritizr::problem(p_dat, usedFeatures, cost_var) %>%
        prioritizr::add_min_set_objective() %>%
        prioritizr::add_relative_targets(targets$target) %>%
        prioritizr::add_binary_decisions() %>%
        prioritizr::add_cbc_solver(verbose = TRUE)

      if (options$lockedInArea != 0) {
        LI <- Dict %>%
          dplyr::filter(categoryID == "LockedInArea") %>%
          dplyr::pull(nameVariable)

        LI_check <- LI %>%  purrr::map(\(x)paste0("input$", paste0("checkLI_", x)))
        LI_sf <- LI %>%  purrr::map(\(x)paste0("raw_sf$", x))

        for (area in 1:length(LI)) {
          if (rlang::eval_tidy(rlang::parse_expr(unlist(LI_check[1])))) {
            browser()
            p1 <- p1 %>%
              prioritizr::add_locked_in_constraints(as.logical(rlang::eval_tidy(rlang::parse_expr(LI_sf[[1]]))))
          }
        }
      }
    } else if (options$obj_func == "min_shortfall") {
        # Add new objective functions
    }
  }

  rm(p_dat)

  return(p1)
}

#' Title
#'
#' @noRd
#'
fupdate_checkbox <- function(session, id_in, Dict, selected = NA){ #works unless everything is not selected

  if (stringr::str_detect(id_in, "check2") == TRUE){
    choice <- Dict %>%
      dplyr::filter(.data$Category  == stringr::str_remove(id_in, "check2")) #%>%
  } else if (stringr::str_detect(id_in, "check") == TRUE){
    choice <- Dict %>%
      dplyr::filter(.data$Category  == stringr::str_remove(id_in, "check")) #%>%
  }
  choice <- choice %>%
    dplyr::select(.data$NameCommon, .data$NameVariable) %>%
    tibble::deframe()

  shiny::updateCheckboxGroupInput(session = session,
                                  inputId = id_in,
                                  choices = choice,
                                  selected = selected)
}

#' Title
#'
#' @noRd
#'
fupdate_checkboxReset <- function(session, id_in, Dict, selected = NA){ #works unless everything is not selected

  if (stringr::str_detect(id_in, "check2") == TRUE){
    choice <- Dict %>%
      dplyr::filter(.data$Category  == stringr::str_remove(id_in, "check2")) #%>%
  } else if (stringr::str_detect(id_in, "check") == TRUE){
    choice <- Dict %>%
      dplyr::filter(.data$Category  == stringr::str_remove(id_in, "check")) #%>%
  }
  choice <- choice %>%
    dplyr::select(.data$NameCommon, .data$NameVariable) %>%
    tibble::deframe()

  # selected <- ifelse(selected == NA, unlist(choice), selected)
  if (is.na(selected)){
    shiny::updateCheckboxGroupInput(session = session, inputId = id_in,
                                    choices = choice,
                                    selected = unlist(choice))
  }

}

#' Reset all inputs to default
#'
#' @noRd
#'
fDeselectVars <- function(session, input, output, id = 1){

  # Add 2 to check ID if using Input2 in the Compare module
  if (id == 2){
    lng <- length(c(input$check2TopPred, input$check2Krill, input$check2Fish, input$check2impBenthic, input$check2Drifter))
    chk <- "2"
  } else {
    lng <- length(c(input$checkTopPred, input$checkKrill, input$checkFish, input$checkimpBenthic, input$checkDrifter))
    chk <- ""
  }

  if (lng < 14){ # More deselected than not
    fupdate_checkboxReset(session, paste0("check",chk,"TopPred"), Dict, selected = NA)
    fupdate_checkboxReset(session, paste0("check",chk,"Krill"), Dict, selected = NA)
    fupdate_checkboxReset(session, paste0("check",chk,"Fish"), Dict, selected = NA)
    fupdate_checkboxReset(session, paste0("check",chk,"impBenthic"), Dict, selected = NA)
    fupdate_checkboxReset(session, paste0("check",chk,"Ice"), Dict, selected = NA)
    fupdate_checkboxReset(session, paste0("check",chk,"Drifter"), Dict, selected = NA)
  } else {
    fupdate_checkbox(session, paste0("check",chk,"TopPred"), Dict, selected = character(0))
    fupdate_checkbox(session, paste0("check",chk,"Krill"), Dict, selected = character(0))
    fupdate_checkbox(session, paste0("check",chk,"Fish"), Dict, selected = character(0))
    fupdate_checkbox(session, paste0("check",chk,"impBenthic"), Dict, selected = character(0))
    fupdate_checkbox(session, paste0("check",chk,"Ice"), Dict, selected = character(0))
    fupdate_checkbox(session, paste0("check",chk,"Drifter"), Dict, selected = character(0))
  }
  rm(lng, chk)
}





# Check the number of features --------------------------------------------

#' Check the number of features
#'
#' @noRd
#'
fCheckFeatureNo <- function(dat){

  f_no <- dat %>%
    dplyr::select(-tidyselect::starts_with("Cost_"), -tidyselect::any_of(c("metric", "cellID"))) %>%
    ncol()

  return(f_no)
}




#' Download Plot - Server Side
#'
#' @noRd
#'
fDownloadPlotServer <- function(input, gg_id, gg_prefix, time_date, width = 19, height = 18) {
  # Create reactiveValues object
  # and set flag to 0 to prevent errors with adding NULL
  rv <- reactiveValues(download_flag = 0)

  dlPlot <- shiny::downloadHandler(

    filename = function() {
      paste("WSMPA2_",gg_prefix,"_", time_date, ".png", sep="")
    },
    content = function(file){
      ggplot2::ggsave(file, plot = gg_id,
                      device = "png", width = width, height = height, units = "in", dpi = 300)
      # When the downloadHandler function runs, increment rv$download_flag
      rv$download_flag <- rv$download_flag + 1

      if(rv$download_flag > 0 & gg_prefix == "Solution"){  # trigger event whenever the value of rv$download_flag changes
        # shinyjs::alert("File downloaded!")
        shinyalert::shinyalert("<h3><strong>Further Information!</strong></h3>", "<h4>Don't forget to also download the data table (Details Tab) to store information about the inputs you provided to this analysis.</h4>",
                               type = "info",
                               closeOnEsc = TRUE,
                               closeOnClickOutside = TRUE,
                               html = TRUE,
                               callbackR = shinyjs::runjs("window.scrollTo(0, 0)"))
        shinyjs::runjs("window.scrollTo(0, 0)")
      }
    })
  # }

  # JDE - Code ready to start puting csv export if needed.
  #else {
  #   dlPlot <- shiny::downloadHandler(
  #     filename = function() {
  #       paste("WSMPA2_",gg_prefix,"_", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv", sep="")
  #     },
  #     content = function(file){
  #
  #       ggplot2::ggsave(file, plot = gg_id,
  #                       device = "png", width = 19, height = 18, units = "in", dpi = 300)
  #     })
  # }
}




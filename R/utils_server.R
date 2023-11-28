




#' Title
#'
#' @noRd
#'
fupdate_checkbox <- function(session, id_in, Dict, selected = NA){ #works unless everything is not selected

  if (stringr::str_detect(id_in, "check2") == TRUE){
    choice <- Dict %>%
      dplyr::filter(.data$category  == stringr::str_remove(id_in, "check2")) #%>%
  } else if (stringr::str_detect(id_in, "check") == TRUE){
    choice <- Dict %>%
      dplyr::filter(.data$category  == stringr::str_remove(id_in, "check")) #%>%
  }
  choice <- choice %>%
    dplyr::select(.data$nameCommon, .data$nameVariable) %>%
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
      dplyr::filter(.data$category  == stringr::str_remove(id_in, "check2")) #%>%
  } else if (stringr::str_detect(id_in, "check") == TRUE){
    choice <- Dict %>%
      dplyr::filter(.data$category  == stringr::str_remove(id_in, "check")) #%>%
  }
  choice <- choice %>%
    dplyr::select(.data$nameCommon, .data$nameVariable) %>%
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
  browser()

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




#' 4features UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_4features_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    tabsetPanel(id = "tabs4",# type = "pills",
                tabPanel("Layer Maps", value = 1,
                         shiny::sidebarLayout(
                           shiny::sidebarPanel(
                             shiny::p("Choose a feature and click 'Show Layer'."),
                             shiny::h2("1. Select Layer"),
                             create_fancy_dropdown(id, Dict, "checkFeat")),

                           # Show a plot of the generated distribution
                           shiny::mainPanel(
                             shiny::p(""), # Add space
                             shiny::htmlOutput(ns("txt_just")),
                             shiny::p(""), # Add space
                             shiny::plotOutput(ns("gg_feat"), height = "700px") %>%
                               shinycssloaders::withSpinner(), #%>%
                             shiny::uiOutput(ns("web_link"))
                           )
                         )
                ),
                tabPanel("Layer Justification", value = 2,
                         shiny::fluidPage(
                           shiny::tableOutput(ns("LayerTable")),
                         )),

    )
  )
}

#' 4features Server Functions
#'
#' @noRd
mod_4features_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    plotFeature <- shiny::reactive({

      # Get the names of the categories in regionalisations
      if (stringr::str_detect(input$checkFeat, "region_"))  {
        # if (input$checkFeat == "region_ech") {
        #   region_names <- c(names_echinoid)
        # }
        # if (input$checkFeat == "region_pel") {
        #   region_names <- c(names_pelagic)
        # }
        # if (input$checkFeat == "region_ben") {
        #   region_names <- c(names_benthic_LayerMod)
        # }
        # if (input$checkFeat == "region_SOBD") {
        #   region_names <- c(names_SOBD)
        # }

        common <- Dict %>%
          dplyr::filter(.data$nameVariable %in% region_names) %>%
          dplyr::select(.data$nameCommon, .data$nameVariable) %>%
          tibble::deframe()

        # Get the columns for regionalisation and pivot longer
        df <- raw_sf %>%
          dplyr::select(tidyselect::all_of(region_names), .data$geometry) %>%
          dplyr::rename(common) %>%
          tidyr::pivot_longer(cols = -.data$geometry, names_to = "region", values_to = "values") %>%
          dplyr::filter(.data$values == 1) %>%
          dplyr::select(-.data$values) %>%
          dplyr::mutate(region = as.factor(.data$region)) %>%
          sf::st_as_sf()

        RegionPlot <- create_regionPlot(df)
        return(RegionPlot)
      } else if (input$checkFeat == "climdat") {
        Bin_plot <- create_climDataPlot(climate_sf)
        return(Bin_plot)
      } else if (startsWith(input$checkFeat, "Cost_")) {

        df <- raw_sf %>%
          sf::st_as_sf() %>%
          dplyr::select(.data$geometry,
                        input$checkFeat) %>%
          dplyr::rename(Cost = input$checkFeat)

        if (input$checkFeat  == "Cost_None"){ #to avoid No Cost Cost
          titleCost <- " "
        } else {
          titleCost <- Dict %>%
            dplyr::filter(.data$nameVariable %in% input$checkFeat) %>%
            dplyr::select(.data$nameVariable, .data$nameCommon) %>%
            tibble::deframe()
          titleCost <- paste0("Rational Use: ",titleCost)
        }

        gg_cost <- create_costPlot(df, titleCost)

        return(gg_cost)

      } else {
        df <- raw_sf %>%
          sf::st_as_sf() %>%
          dplyr::select(.data$geometry,
                        input$checkFeat) %>%
          dplyr::rename(pred_bin = input$checkFeat)


        ## TODO Decide how to plot based on the data type - Featurres, Bioregional,
        # Maybe Dictionary column called "Binary, Continuous"?
        # Then also by feature or cost or climate.....

        df <-  df %>%
          dplyr::mutate(pred_bin = dplyr::if_else(.data$pred_bin == 1, "Suitable", "Not Suitable"),
                        pred_bin = factor(.data$pred_bin, levels = c("Suitable", "Not Suitable")))

        Bin_plot <- create_binPlot(df, df$pred_bin, title = "Planning Units",
                                   values = c("Suitable" = "#3182bd", "Not Suitable" = "#c6dbef"))

        return(Bin_plot)
      }
    }) %>% shiny::bindCache(input$checkFeat)


    output$gg_feat <- shiny::renderPlot({
      plotFeature()
    }) %>% shiny::bindCache(input$checkFeat)


    # Feature justification table
    output$LayerTable <- shiny::renderTable(
      Dict %>%
        dplyr::filter(.data$includeJust == TRUE) %>%
        dplyr::select(.data$category, .data$nameCommon, .data$justification) %>%
        dplyr::arrange(.data$category, .data$nameCommon)
    )

    # Text justification for the spatial plot
    output$txt_just <- shiny::renderText(
      Dict %>%
        dplyr::filter(.data$nameVariable == input$checkFeat) %>%
        dplyr::pull(.data$justification)
    )

  })
}



## To be copied in the UI
# mod_4features_ui("4features_ui_1")

## To be copied in the server
# mod_4features_server("4features_ui_1")

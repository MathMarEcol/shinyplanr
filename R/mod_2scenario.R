#' 2scenario UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom rlang .data
mod_2scenario_ui <- function(id) {
  ns <- shiny::NS(id)

  Vars <- fcreate_vars(id = id, Dict = Dict, name_check = "sli_")
  shinyjs::useShinyjs()

  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h2("1. Select Features and Targets"),
        shiny::actionButton(ns("deselectVars"), "Reset All Features",
          width = "100%", class = "btn btn-outline-primary",
          style = "display: block; margin-left: auto; margin-right: auto; padding:4px; font-size:120%"
        ),
        purrr::pmap(Vars, fcustom_slider),
        shiny::h2("2. Select Rational Use"),
        fcustom_cost(id, "costid", Dict),
        shinyjs::hidden(div(
          id = ns("switchClimSmart"),
          shiny::h2("3. Climate-resilient"),
          shiny::p("Should the spatial plan be made climate-resilient?"),
          shiny::checkboxInput(ns("checkClimsmart"), "Make Climate-resilient", FALSE)
        )),
        # shiny::conditionalPanel(
        #   condition = "options$climate_change == TRUE",
        #   shiny::h2("3. Climate-resilient"),
        #   shiny::p("Should the spatial plan be made climate-resilient?"),
        #   shiny::checkboxInput(ns("checkClimsmart"),"Make Climate-resilient", FALSE),
        # ),
        shiny::br(), # Leave space for analysis button at bottom
        shiny::br(), # Leave space for analysis button at bottom
        shiny::fixedPanel(
          style = "z-index:100", # To force the button above all plots.
          shiny::actionButton(ns("analyse"), "Run Analysis", shiny::icon("paper-plane"),
            width = "100%", class = "btn btn-primary",
            style = "display: block; float: left; padding:4px; font-size:150%;"
          ),
          right = "71%", bottom = "1%", left = "5%"
        ),
      ),
      shiny::mainPanel(
        shinydisconnect::disconnectMessage(
          text = "Your session timed out, reload the application.",
          refresh = "Reload now",
          background = "#f89f43",
          colour = "white",
          overlayColour = "grey",
          overlayOpacity = 0.3,
          refreshColour = "brown"
        ),
        shinyjs::useShinyjs(),
        tabsetPanel(
          id = ns("tabs"), # type = "pills",
          tabPanel("Scenario",
            value = 1,
            shiny::fixedPanel(
              style = "z-index:100", # To force the button above all plots.=
              shiny::downloadButton(ns("dlPlot1"), "Download Plot",
                style = "float: right; padding:4px; font-size:120%"
              ),
              right = "1%", bottom = "1%", left = "34%"
            ),
            shiny::span(shiny::h2(shiny::textOutput(ns("hdr_soln")))),
            shiny::textOutput(ns("txt_soln")),
            shinycssloaders::withSpinner(shiny::plotOutput(ns("gg_soln"), height = "700px"))
          ),
          tabPanel("Targets",
            value = 2,
            shiny::fixedPanel(
              style = "z-index:100", # To force the button above all plots.
              shiny::downloadButton(ns("dlPlot2"), "Download Plots",
                style = "float: right; padding:4px; font-size:120%"
              ),
              right = "1%", bottom = "1%", left = "34%"
            ),
            shiny::span(shiny::h2(shiny::textOutput(ns("hdr_target")))),
            shiny::textOutput(ns("txt_target")),
            shiny::br(),
            shinycssloaders::withSpinner(shiny::plotOutput(ns("gg_TargetPlot"), height = "600px"))
          ),
          tabPanel("Rational Use",
            value = 3,
            shiny::fixedPanel(
              style = "z-index:100", # To force the button above all plots.
              shiny::downloadButton(ns("dlPlot3"), "Download Plot",
                style = "float: right; padding:4px; font-size:120%"
              ),
              right = "1%", bottom = "1%", left = "34%"
            ),
            shiny::span(shiny::h2(shiny::textOutput(ns("hdr_cost")))),
            shiny::textOutput(ns("txt_cost")),
            shinycssloaders::withSpinner(shiny::plotOutput(ns("gg_cost"), height = "700px"))
          ),
          # tabPanel("Selection Frequency", value = 5,
          #          shiny::fixedPanel(style="z-index:100", # To force the button above all plots.
          #                            shiny::downloadButton(ns("dlPlot5"), "Download Plot",
          #                                                  style = "float: right; padding:4px; font-size:120%"),
          #                            right = '1%', bottom = '1%', left = '34%'),
          #          shiny::br(),
          #          shiny::actionButton(ns("plotSelFreq"), "Show Selection Frequency", align = "center",
          #                              style = "display: block; margin-left: auto; margin-right: auto; padding:4px; font-size:120%"),
          #          shiny::p("WARNING: This will take 1-5 minutes to run. Please don't press the button several times or navigate away from this page while the analysis is running.", align = "center"),
          #          shiny::span(shiny::h2(shiny::textOutput(ns("hdr_selFreq")))),
          #          shiny::textOutput(ns("txt_selFreq")),
          #          shinycssloaders::withSpinner(shiny::plotOutput(ns("gg_selFreq"), height = "700px"))),
          tabPanel("Climate Resilience",
            value = 6,
            shiny::fixedPanel(
              style = "z-index:100", # To force the button above all plots.
              shiny::downloadButton(ns("dlPlot6"), "Download Plot",
                style = "float: right; padding:4px; font-size:120%"
              ),
              right = "1%", bottom = "1%", left = "34%"
            ),
            shiny::span(shiny::h2(shiny::textOutput(ns("hdr_clim")))),
            shiny::textOutput(ns("txt_clim")),
            shinycssloaders::withSpinner(shiny::plotOutput(ns("gg_clim"), height = "700px"))
          ),
          tabPanel("Details",
            value = 7,
            shiny::fixedPanel(
              style = "z-index:100", # To force the button above all plots.=
              shiny::downloadButton(ns("dlPlot7"), "Download Table",
                style = "float: right; padding:4px; font-size:120%"
              ),
              right = "1%", bottom = "1%", left = "34%"
            ),
            shiny::span(shiny::h2(shiny::textOutput(ns("hdr_DetsSummary")))),
            shiny::br(),
            shiny::tableOutput(ns("SummaryTable")),
            shiny::span(shiny::h2(shiny::textOutput(ns("hdr_DetsData")))),
            shiny::tableOutput(ns("DataTable"))
          ),
        )
      )
    )
  )
}

#' 2scenario Server Functions
#'
#' @noRd
mod_2scenario_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (options$climate_change != 0) { # dont make observeEvent because it's a global variable
      shinyjs::show(id = "switchClimSmart")
    }

    observeEvent(input$disconnect, {
      session$close()
    })

    # # Go back to the first tab when analyse is clicked.
    shiny::observeEvent(input$analyse, {
      shiny::updateTabsetPanel(session, "tabs", selected = 1)
    })

    # Go back to the top of the page when analyse is clicked.
    shiny::observeEvent(input$analyse, {
      shinyjs::runjs("window.scrollTo(0, 0)")
    })

    # Reset values
    shiny::observeEvent(input$reset,
      {
        fResetInputs(session, input, output)
      },
      ignoreInit = TRUE
    )

    # Deselect features
    shiny::observeEvent(input$deselectVars,
      {
        fDeselectVars(session, input, output)
      },
      ignoreInit = TRUE
    )


    # Return targets and names for all features from sliders ---------------------------------------------------

    p1Data <- shiny::reactive({ ####### SPLIT in target, p, s (maybe features?)

      f <- vars[stringr::str_detect(vars, "Cost_", negate = TRUE)]

      targets <- f %>%
        purrr::map(\(x) rlang::eval_tidy(rlang::parse_expr(paste0("input$", paste0("sli_", x))))) %>%
        tibble::enframe() %>%
        tidyr::unnest(cols = value) %>%
        dplyr::rename(features = name, targets = value) %>%
        dplyr::mutate(features = f)


      out_sf <- raw_sf %>%
        dplyr::select(
          .data$geometry,
          tidyselect::all_of(targets$features),
          tidyselect::starts_with("Cost_")
        ) %>%
        sf::st_as_sf()

      if (input$checkClimsmart == FALSE) {
        p_dat <- out_sf
      } else if (input$checkClimsmart == TRUE) {
        featuresDF <- out_sf %>%
          dplyr::select(
            .data$geometry,
            tidyselect::all_of(targets$features)
          ) %>%
          dplyr::mutate(cellID = 1:nrow(.))

        targets <- targets %>% # TODO: take renaming out and streamline between spatialplanr and shinyplanrS
          dplyr::rename(
            feature = features,
            target = targets
          )

        if (options$climate_change == 1) { # CPA approach

          targets <- targets %>%
            dplyr::mutate(target = target / 100) #requires number between 0-1

          CS_Approach <- spatialplanr::splnr_climate_priorityAreaApproach(
            featuresDF = featuresDF,
            metricDF = climate_sf, targetsDF = targets, direction = 1, refugiaTarget = 1
          )
        } else if (options$climate_change == 2) { # feature approach

          CS_Approach <- spatialplanr::splnr_climate_featureApproach(
            featuresDF = featuresDF,
            metricDF = climate_sf, targetsDF = targets, direction = 1, refugiaTarget = 30 #here: not 0.3 but 30 needed -> need to streamline; also: what to set as default for precentile and target here?
          )
        } else if (options$climate_change == 3) { # percentile approach

          CS_Approach <- spatialplanr::splnr_climate_percentileApproach(
            featuresDF = featuresDF,
            metricDF = climate_sf, targetsDF = targets, direction = 1 #default: 35%
          )
        }

        targets <- CS_Approach$Targets %>% # TODO: take renaming out and streamline between spatialplanr and shinyplanrS
          dplyr::rename(
            targets = target,
            features = feature
          ) %>%
          dplyr::mutate(targets = targets * 100)


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
        print("Something odd is going on here. Check climat-smart tick box.")
      }

      f_no <- fCheckFeatureNo(p_dat)

      if (f_no == 1) {
        shinyalert::shinyalert("Error", "No features have been selected. You can't run a spatial prioritization without any features.",
          type = "error",
          callbackR = shinyjs::runjs("window.scrollTo(0, 0)")
        )

        p_dat <- p_dat %>%
          dplyr::mutate(DummyVar = 1)

        p1 <- prioritizr::problem(p_dat, "DummyVar", input$costid) %>%
          prioritizr::add_min_set_objective() %>%
          prioritizr::add_relative_targets(0) %>%
          prioritizr::add_binary_decisions() %>%
          prioritizr::add_cbc_solver(verbose = TRUE)
      } else {
        ## Get names of the features
        if (input$checkClimsmart == TRUE) {
          usedFeatures <- p_dat %>%
            sf::st_drop_geometry() %>%
            dplyr::select(-tidyselect::starts_with("Cost_"), -.data$cellID, -.data$metric) %>%
            names()
        } else {
          usedFeatures <- targets$features
        }


        p1 <- prioritizr::problem(p_dat, usedFeatures, input$costid) %>%
          prioritizr::add_min_set_objective() %>%
          prioritizr::add_relative_targets(targets$targets / 100) %>%
          prioritizr::add_binary_decisions() %>%
          prioritizr::add_cbc_solver(verbose = TRUE)
        # }
      }

      rm(p_dat)

      return(p1)
    })


    # Solve the problem -------------------------------------------------------
    selectedData <- shiny::reactive({
      # browser()
      selectedData <- solve(p1Data(), run_checks = FALSE) %>%
        sf::st_as_sf()
      return(selectedData)
    }) %>% shiny::bindEvent(input$analyse)


    analysisTime <- shiny::reactive({
      analysisTime <- format(Sys.time(), "%Y%m%d%H%M%S")
    }) %>% shiny::bindEvent(input$analyse)


    ############## All Plots #########################


    ## Binary Solution Plot ----------------------------------------------------


    observeEvent(
      {
        input$tabs == 1
      },
      {
        # Solution plotting reactive
        plot_data1 <- shiny::reactive({
          soln_text <- fSolnText(input, selectedData())

          plot1 <- spatialplanr::splnr_plot_binFeature(df = selectedData(), colInterest = selectedData()$solution_1, plotTitle = "Planning Units") +
            ggplot2::annotate(geom = "text", label = soln_text[[1]], x = Inf, y = Inf, hjust = 1.05, vjust = 1.5) +
            spatialplanr::splnr_gg_add(
              # Bndry = Bndry,
              land = landmass,
              cropLand = selectedData(),
              ggtheme = map_theme
            )

          if (input$costid != "Cost_None") {
            plot1 <- plot1 +
              ggplot2::annotate(geom = "text", label = soln_text[[2]], x = Inf, y = Inf, hjust = 1.03, vjust = 3.5)
          } else {
            plot1 <- plot1
          }
          return(plot1)
        })

        output$gg_soln <- shiny::renderPlot({
          plot_data1()
        }) %>%
          shiny::bindEvent(input$analyse)

        hdrr_soln <- shiny::reactive({
          txt_out <- "Your Scenario"
          return(txt_out)
        })



        output$hdr_soln <- shiny::renderText({
          hdrr_soln()
        }) %>%
          shiny::bindEvent(input$analyse)

        output$txt_soln <- shiny::renderText({
          paste(
            "This plot shows the optimal planning scenario for the study area
              that meets the selected targets for the chosen features whilst
              minimising the rational use. The categorical map displays, which of
              the hexagonal planning units were selected as important for meeting
              the conservation targets (dark blue) and which were not selected (light blue)
              either due to not being in an area prioritized for the selected features or
              because they are within areas valuable and accessible for other rational uses.
              For the chosen inputs.",
            round((selectedData() %>% dplyr::filter(.data$solution_1 == "Selected") %>% # TODO probably should adjust the txt function to return numerics to use here
              nrow() / nrow(selectedData())) * 100),
            "% of the planning region was selected."
          )
        }) %>%
          shiny::bindEvent(input$analyse)
      }
    )






    ## Target Plot -------------------------------------------------------------


    observeEvent(
      {
        input$tabs == 2
      },
      {
        gg_Target <- shiny::reactive({
          targetPlotData <- spatialplanr::splnr_get_featureRep(soln = selectedData(), pDat = p1Data(), climsmart = input$checkClimsmart)

          # TODO Consider replacing category with Dict. In fact we can just make category a binary as Dict is available everywhere.
          gg_Target <- spatialplanr::splnr_plot_featureRep(targetPlotData, category = category, nr = 2, showTarget = TRUE)

          return(gg_Target)
        }) %>%
          # shiny::bindCache(input$checkTopPred, input$checkFish, input$checkKrill, input$checkDrifter, input$checkLockIn, input$checkIce,
          #                  input$checkRegion, input$checkimpBenthic, input$checkClimsmart, input$slider_imp, input$slider_rep, input$costid, input$checkResBlock) #%>%
          shiny::bindEvent(input$analyse)


        output$gg_TargetPlot <- shiny::renderPlot({
          gg_Target()
        }) %>%
          shiny::bindEvent(input$analyse)

        output$hdr_target <- shiny::renderText({
          "Targets"
        })

        output$txt_target <- shiny::renderText({
          "Given the scenario for the spatial planning problem formulated with
      the chosen inputs, these plots show the proportion of
      suitable habitat/area of each of the important and representative
      conservation features that are included. The dashed line represents
      the set target for the features. Hollow bars with a black border indicate incidental
        protection of features which were not chosen in this analysis but have areal overlap with selected planning units."
        })
        # shiny::bindCache(input$checkTopPred, input$checkFish, input$checkKrill, input$checkDrifter, input$checkLockIn, input$checkIce,
        #                  input$checkRegion, input$checkimpBenthic, input$checkClimsmart, input$slider_imp, input$slider_rep, input$costid, input$checkResBlock) #%>%
        # shiny::bindEvent(input$analyse)


        output$dlPlot2 <- fDownloadPlotServer(input, gg_id = gg_Target(), gg_prefix = "Target", time_date = analysisTime()) # Download figure
      }
    ) # end observeEvent 2





    ## Cost Plot -------------------------------------------------------------

    observeEvent(
      {
        input$tabs == 3
      },
      {
        costPlotData <- shiny::reactive({
          spatialplanr::splnr_plot_costOverlay(selectedData(),
            Cost = NA, Cost_name = input$costid,
            legendTitle = "Cost",
            plotTitle = "Solution overlaid with cost"
          ) +
            spatialplanr::splnr_gg_add(
              # Bndry = Bndry,
              land = landmass,
              cropLand = selectedData(),
              ggtheme = map_theme
            )
        }) %>%
          shiny::bindEvent(input$analyse)


        output$gg_cost <- shiny::renderPlot({
          costPlotData()
        }) %>%
          shiny::bindEvent(input$analyse)

        output$hdr_cost <- shiny::renderText({
          "Rational Use Overlaid with Selection"
        }) %>%
          shiny::bindEvent(input$analyse)


        # TODO Move this text to the Dictionary and implement call to display here as usual
        output$txt_cost <- shiny::renderText({
          if (input$costid == "Cost_Total" || input$costid == "Cost_Krill" || input$costid == "Cost_Toothfish") {
            cost_txt <- paste("For the chosen input, the rational use is low in areas with low predicted Antarctic krill
                          and/or toothfish abundances (light orange). The rational use is high in planning units with
                          suitable Antarctic krill/toothfish habitat that would be lost to fishing if a
                          particular planning unit was included in a protected area (dark orange).")
          } else if (input$costid == "Cost_IceA") {
            cost_txt <- paste("For the chosen input, the rational use is low where the area is inaccessible to fishing
                          due to ice coverage (light orange). The rational use is high in planning units with low ice
                          area, i.e. area is accessible (dark orange).")
          } else if (input$costid == "Cost_None") {
            cost_txt <- paste("For the chosen input, there is no rational use. The prioritisation minimizes the area
                          that is selected in the scenario.")
          }
          paste0("To illustrate how the chosen rational use influences the spatial plan, this plot shows the
             spatial plan (= scenario) overlaid with the rational use of including a planning unit in a
             reserve. ", cost_txt)
        }) %>%
          shiny::bindEvent(input$analyse)

        output$dlPlot3 <- fDownloadPlotServer(input, gg_id = costPlotData(), gg_prefix = "RationalUse", time_date = analysisTime()) # Download figure
      }
    ) # end observeEvent 3









    ## Climate Resilience Plot -------------------------------------------------


    observeEvent(
      {
        input$tabs == 6
      },
      {
        ggr_clim <- shiny::reactive({
          ggClimDens <- spatialplanr::splnr_plot_climKernelDensity(
            soln = list(selectedData()),
            names = c("Input 1"), type = "Normal",
            legendTitle = "Climate resilience metric (add unit)",
            xAxisLab = "Climate resilience metric"
          )
          # s1 <- selectedData() %>%
          #   dplyr::mutate(solution_1 = dplyr::if_else(.data$solution_1 == "Selected", 1, 0)) %>%
          #   tibble::as_tibble()
          #
          # ggr_clim <- create_climKernelDensityPlot(s1) # create_climKernelDensityPlot(s1)
          return(ggClimDens)
        }) %>%
          shiny::bindEvent(input$analyse)

        output$gg_clim <- shiny::renderPlot({
          if (input$checkClimsmart == TRUE) {
            ggr_clim()
          }
        }) %>%
          shiny::bindEvent(input$analyse)

        output$hdr_clim <- shiny::renderText({
          if (input$checkClimsmart == TRUE) {
            paste("Climate Resilience")
          }
        }) %>%
          shiny::bindEvent(input$analyse)

        output$txt_clim <- shiny::renderText({
          if (input$checkClimsmart == TRUE) {
            paste("Kernel density estimates for the climate-resilience metric. The metric comprises two components,
          both based on projected temperature in 2100 from a suite of Earth System Models under a high emission scenario:
          1. Exposure to climate change (amount of warming); 2. Climate velocity (the pace of isotherm movement).
          These two components are combined into a single climate-resilience metric so that higher values represent areas
          likely to warm less and where biodiversity is more likely to be retained. The prioritization preferentially places protected areas
          where there are higher values of the climate-resilience metric, whilst still meeting the biodiversity targets and
          minimising overlap with rational use areas. The dark blue polygon represents the climate-resilience metric in planning units
          selected for protection. The light blue polygon represents the climate-resilience metric in areas not selected for protection. The median values of the climate-resilience metric for the two groups are represented by the vertical lines.")
          } else if (input$checkClimsmart == FALSE) {
            paste("Climate-smart spatial planning option not selected.")
          }
        }) %>%
          shiny::bindEvent(input$analyse)

        output$dlPlot6 <- fDownloadPlotServer(input, gg_id = ggr_clim(), gg_prefix = "Climate", time_date = analysisTime()) # Download figure
      }
    ) # end observeEvent 6





    # Table of Targets --------------------------------------------------------

    observeEvent(
      {
        input$tabs == 7
      },
      {
        DataTabler <- shiny::reactive({
          targetPlotData <- spatialplanr::splnr_get_featureRep(soln = selectedData(), pDat = p1Data(), climsmart = input$checkClimsmart)

          # TODO Add category to spatialplanr::splnr_get_featureRep and remove from splnr_plot_featureRep
          FeaturestoSave <- targetPlotData %>%
            dplyr::left_join(Dict %>% dplyr::select(.data$NameVariable, .data$Category), by = c("feature" = "NameVariable")) %>%
            dplyr::mutate(value = as.integer(round(.data$relative_held * 100))) %>%
            dplyr::select(.data$Category, .data$feature, .data$target, .data$value, .data$incidental) %>%
            dplyr::rename(
              Feature = .data$feature,
              `Protection (%)` = .data$value,
              `Target (%)` = .data$target,
              Incidental = .data$incidental
            ) %>%
            dplyr::arrange(.data$Category, .data$Feature)

          return(FeaturestoSave)
        }) %>%
          shiny::bindEvent(input$analyse)

        output$DataTable <- shiny::renderTable({
          DataTabler()
        }) %>%
          shiny::bindEvent(input$analyse)


        SummaryTabler <- shiny::reactive({
          CosttoSave <- Dict %>%
            dplyr::filter(.data$NameVariable %in% input$costid)

          SummarytoSave <- tibble::tribble(
            ~`Rational Use`, ~`Climate Smart`,
            CosttoSave$NameCommon, dplyr::if_else(input$checkClimsmart, "Yes", "No")
          )

          return(SummarytoSave)
        }) %>%
          shiny::bindEvent(input$analyse)


        output$SummaryTable <- shiny::renderTable({
          SummaryTabler()
        }) %>%
          shiny::bindEvent(input$analyse)

        output$hdr_DetsSummary <- shiny::renderText("General")
        output$hdr_DetsData <- shiny::renderText("Feature Summary")

        # Create data tables for download
        ggr_DataPlot <- shiny::reactive({
          dat <- DataTabler() %>%
            dplyr::mutate(Class = as.factor(.data$Class)) %>%
            dplyr::group_by(.data$Class) %>%
            dplyr::group_split()

          design <- "AACC
           BBCC
           BBCC
           BBCC"

          ggr_DataPlot <- patchwork::wrap_plots(
            gridExtra::tableGrob(SummaryTabler(), rows = NULL, theme = gridExtra::ttheme_default(base_size = 12)),
            gridExtra::tableGrob(dat[[1]], rows = NULL, theme = gridExtra::ttheme_default(base_size = 8)),
            gridExtra::tableGrob(dat[[2]], rows = NULL, theme = gridExtra::ttheme_default(base_size = 8)),
            design = design
          )

          return(ggr_DataPlot)
        }) %>%
          shiny::bindEvent(input$analyse)

        output$dlPlot7 <- fDownloadPlotServer(input, gg_id = ggr_DataPlot(), gg_prefix = "DataSummary", time_date = analysisTime(), width = 16, height = 10) # Download figure
      }
    ) # End observe event 7
  })
}

## To be copied in the UI
# mod_2scenario_ui("2scenario_1")

## To be copied in the server
# mod_2scenario_server("2scenario_1")

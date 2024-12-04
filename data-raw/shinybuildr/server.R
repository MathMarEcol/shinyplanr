
# Define server logic
function(input, output, session) {

  roptions <- shiny::reactive({

    # APP PARAMETERS --------------------------------------------------
    options <- list(

      ## General Options
      nav_title = input$nav_title, # Navbar title
      nav_primary = input$nav_primary,

      ## File locations
      file_logo = input$file_logo,

      ## App Setup Options

      #switch modules on/off
      mod_1welcome = "mod_1welcome" %in% input$modules,
      mod_2scenario = "mod_2scenario" %in% input$modules,
      mod_3compare = "mod_3compare" %in% input$modules,
      mod_4features = "mod_4features" %in% input$modules,
      mod_6help = "mod_6help" %in% input$modules,
      mod_7credit = "mod_7credit" %in% input$modules,

      # TODO Get this working - get conditional panels working for global variables
      climate_change = input$radio_climate, #switch climate change on/off; 0 = not clim-smart; 1 = CPA; 2 = Feature; 3 = Percentile
      # Warning: still requires some changes in the app: direction, percentile etc. should this be in here? those are input options to the functions

      lockedInArea = input$lockedInArea, #Includes locked in areas

      ## Which objective function module are we using
      obj_func = input$obj_func,

      ## Geographic Options
      cCRS = input$crs

    )
    return(options)

  })


  ## Load Boundary
  dict <- shiny::reactive({
    fread_file(input, inputId = "dict_file", ext = "csv")
  })


  output$data_dict <- DT::renderDataTable(
    dict()
  )


  ## Load Data files
  dat <- shiny::reactive({

    dat <- fread_file(input, inputId = "data_file", ext = "rds")
#
#     dat <- dat %>%
#       sf::st_drop_geometry() %>%
#       dplyr::select(tidyselect::all_of(vars)) %>%
#       dplyr::select(which(!colSums(., na.rm=TRUE) %in% 0)) %>% # Remove all zero columns
#       dplyr::bind_cols(dat %>% dplyr::select(geometry)) # Add geometry back in

    return(dat)

  })


  output$data_tbl <- shiny::renderText({
    colnames(dat())
  })


  ## Load Boundary
  bndry <- shiny::reactive({
    fread_file(input, inputId = "bndry_file", ext = "rds")
  })


  ## Load Coastline
  coast <- shiny::reactive({
    fread_file(input, inputId = "coast_file", ext = "rds")
  })


  output$save_button <- shiny::downloadHandler(

    #TODO See if I can get a directory included in this.....
    filename = function() {
      file = "sysdata.rda"
    },
    content = function(file){

      options <- roptions()
      raw_sf <- dat() # TODO Change this to data_sf in the app. Then here. The name makes more sense
      bndry <- bndry()
      overlay <- coast()
      Dict <- dict()

      vars <- Dict %>%
        dplyr::filter(!type %in% c("Justification")) %>%
        dplyr::pull(nameVariable)

      # Check if variables were removed from the data due to zero columns
      if (length(vars) != dim(raw_sf)[2]-1){
        stop("raw_sf and the Dictionary have different numbers of variables. If columns
       were removed due to being all zero above, please remove corresponding
       variable from Dict")}

      # TODO Write a check function that ensures everyting is accounted for....

      save(options,
           # map_theme,
           # bar_theme,
           Dict,
           vars,
           raw_sf,
           bndry,
           overlay,
           # tx_1welcome,
           # tx_6faq,
           # tx_6technical,
           # tx_6changelog,
           # tx_6references,
           # tx_7credit,
           file = file)

    })


  #   # Copy logo to required directory
  #   file.copy(options$file_logo, file.path("inst", "app", "www", "logo.png"), overwrite = TRUE)
  #
  #
  #   # DATASETS --------------------------------------------------------
  #
  #   # A dictionary of all data and feature-specific set up values
  #   Dict <- readr::read_csv(file.path(data_dir, "Dict_Feature.csv")) %>%
  #     dplyr::filter(includeApp) %>% # Only those features to be included
  #     dplyr::arrange(.data$type, .data$category, .data$nameCommon)

  #
  #   raw_sf <- dat_sf %>%
  #     sf::st_drop_geometry() %>%
  #     dplyr::select(tidyselect::all_of(vars)) %>%
  #     dplyr::select(which(!colSums(., na.rm=TRUE) %in% 0)) %>% # Remove all zero columns
  #     dplyr::bind_cols(dat_sf %>% dplyr::select(geometry)) # Add geometry back in
  #
  #   # Check if variables were removed from the data due to zero columns
  #   if (length(vars) != dim(raw_sf)[2]-1){
  #     stop("raw_sf and the Dictionary have different numbers of variables. If columns
  #        were removed due to being all zero above, please remove corresponding
  #        variable from Dict")}
  #
  #
  #
  #   # Plotting Overlays -------------------------------------------------------
  #

  #
  #   # TODO Work out how to add options here without having to define all.
  #   # Change to a list called plot_options()? that is passed to the function.
  #
  #
  #   # MODULE 1 - WELCOME ------------------------------------------------------
  #   tx_1welcome <- readr::read_file(file.path(data_dir, "html_1welcome.txt"))
  #
  #   # return_list <- read_textboxes(FILENAME)
  #
  #
  #
  #   # MODULE 2 - SCENARIO ------------------------------------------------------
  #
  #
  #
  #   # MODULE 3 - COMPARISON ------------------------------------------------------
  #
  #
  #
  #   # MODULE 6 - HELP ------------------------------------------------------
  #   tx_6faq <- readr::read_file(file.path(data_dir, "html_6faq.txt"))
  #   tx_6changelog <- readr::read_file(file.path(data_dir, "html_6changelog.txt"))
  #   tx_6technical <- readr::read_file(file.path(data_dir, "html_6technical.txt"))
  #   tx_6references <- readr::read_file(file.path(data_dir, "html_6references.txt"))
  #
  #
  #   # MODULE 7 - CREDIT ------------------------------------------------------
  #   tx_7credit <- readr::read_file(file.path(data_dir, "html_7credit.txt"))
  #
  #
  #
  #   # HEX STICKER -------------------------------------------------------------
  #   # Create app-specific Hex sticker if wanted. Otherwise the generic shinyplanr one will be used
  #   hexSticker::sticker(options$file_logo,
  #                       package="",
  #                       s_x=1,
  #                       s_y=1,
  #                       s_width=.8,
  #                       h_fill = "#0033a0",
  #                       h_color = "grey40",
  #                       # url = "",
  #                       # u_color = "white",
  #                       dpi=600,
  #                       filename=file.path("inst", "app", "www", "Hex.png"))
  #
  #   golem::use_favicon(file.path("inst", "app", "www", "Hex.png"), pkg = golem::get_golem_wd(), method = "curl")
  #
  #
  #   # PLOTTING THEME -----------------------------------------------------------
  #   map_theme <- list(
  #     ggplot2::theme_bw(),
  #     ggplot2::theme(
  #       legend.position = "right",
  #       legend.direction = "vertical",
  #       # text = ggplot2::element_text(size = 6, colour = "black"),
  #       axis.text = ggplot2::element_text(size = 9, colour = "black"),
  #       plot.title = ggplot2::element_text(size = 12),
  #       legend.title = ggplot2::element_text(size = 9),
  #       legend.text = ggplot2::element_text(size = 9),
  #       axis.title = ggplot2::element_blank()
  #     )
  #   )
  #
  #   bar_theme <- list(
  #     ggplot2::theme_bw(),
  #     ggplot2::theme(
  #       legend.position = "right",
  #       legend.direction = "vertical",
  #       # text = ggplot2::element_text(size = 6, colour = "black"),
  #       axis.text = ggplot2::element_text(size = 6, colour = "black"),
  #       plot.title = ggplot2::element_text(size = 12),
  #       legend.title = ggplot2::element_text(size = 9),
  #       legend.text = ggplot2::element_text(size = 9),
  #       axis.title = ggplot2::element_blank()
  #     )
  #   )
  #
  #
  #
  #
  #   usethis::use_data(options,
  #                     map_theme,
  #                     bar_theme,
  #                     Dict,
  #                     vars,
  #                     raw_sf,
  #                     climate_sf,
  #                     bndry,
  #                     overlay,
  #                     tx_1welcome,
  #                     tx_6faq,
  #                     tx_6technical,
  #                     tx_6changelog,
  #                     tx_6references,
  #                     tx_7credit,
  #                     overwrite = TRUE,
  #                     internal = TRUE)
  #
  #
  #

}

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(colourpicker)
library(DT)

source("utils.R")

# Define UI for application that draws a histogram
shiny::fluidPage(

  shiny::fluidRow(
    # Application title
    shiny::column(9,
                  shiny::titlePanel(shiny::HTML("Create new region-specific <i>shinyplanr</i> app")),
                  shiny::HTML("This shiny app provides the framework for the development of new
                              region/country specific <i>shinyplanr</i> apps. <br>"),
    ),
    shiny::column(2,
                  shiny::img(src = "shinyplanr.png", width = "90%", style = "float:left; padding: 10px 10px 10px 10px;"),
    ),
    style = "background-color:#5283B5; color:white",
  ),


  shiny::h3("App Setup"),
  shiny::p("Select the options for the look of your shinyplanr app including tabs, primary colours, titles, font type and base size. "),
  shiny::fluidRow(
    shiny::column(6,
                  shiny::textInput(inputId = "nav_title",
                                   label = "NavBar App Title",
                                   value = "Spatial Planning in Australia")),
    shiny::column(6,
                  #switch modules on/off
                  shiny::checkboxGroupInput(inputId = "modules",
                                            label = "Select shinyplanr modules to include in app",
                                            choices =  c("Welcome" = "mod_1welcome",
                                                         "Scenario" = "mod_2scenario",
                                                         "Compare" = "mod_3compare",
                                                         "Features" = "mod_4features",
                                                         "Help" = "mod_6help",
                                                         "Credit" = "mod_7credit"),
                                            inline = TRUE,
                                            selected = c("mod_1welcome", "mod_2scenario", "mod_3compare",
                                                         "mod_4features", "mod_6help", "mod_7credit"))),
  ),
  shiny::fluidRow(
    shiny::column(6,
                  colourpicker::colourInput(inputId = "nav_primary",
                                            label = "Select Primary App Colour",
                                            value = "#2C3E50")),
  ),

  ## File locations
  shiny::fileInput(inputId = "file_logo",
                   label = "Select App Logo",
                   multiple = FALSE,
                   accept = "image/*",
                   buttonLabel = "Browse...",
                   placeholder = "No file selected"),

  shiny::hr(),

  shiny::h3("Geographic Data Setup"),
  shiny::fluidRow(
    shiny::column(6,
                  shiny::textInput(inputId = "country",
                                   label = "Country Name",
                                   value = "Australia")),
    shiny::column(6,
                  shiny::textInput(inputId = "crs",
                                   label = "EPSG Code for projection",
                                   value = "ESRI:54009")),
  ),

  shiny::hr(),

  shiny::h3("Prioritisation Setup"),
  shiny::fluidRow(
    shiny::column(6,
                  shiny::selectInput(inputId = "obj_func",
                                     label = "Select Objective Function",
                                     choices = c("Minimum Set" = "min_set",
                                                 "Minimum Shortfall" = "min_shortfall"),
                                     selected = "min_set",
                                     multiple = FALSE),
    ),
    shiny::column(6,
                  shiny::checkboxInput(inputId = "lockedInArea",
                                       label = "Include Locked-in Areas",
                                       value = FALSE)),
  ),

  shiny::hr(),

  shiny::h3("Climate-smart Setup"),
  shiny::fluidRow(
    shiny::column(4,
                  shiny::radioButtons(inputId = "radio_climate",
                                      label = c("Climate-Smart Options"),
                                      choices = c("None" = 0, "CPA Approach" = 1, "Feature Approach" = 2, "Percentile Approach" = 3),
                                      inline = TRUE),
    ),

    shiny::conditionalPanel(
      condition = paste0("input.radio_climate > 0"),
      shiny::column(3,
                    shiny::numericInput(inputId = "percentile",
                                        label = "Percentile of Climate-Smart protection",
                                        value = 35, #fget_percentile(input$radio_climate),
                                        min = 0,
                                        max = 100,
                                        step = 1),
      ),

      shiny::column(5,
                    shiny::selectInput(inputId = "direction",
                                       label = "What signifies more climate-Smart areas are",
                                       choices = c("Climate-Smart at high values" = 1,
                                                   "Climate-Smart at low values" = -1)),
      ),
    ),
  ),

  shiny::hr(),


  ## File locations
  shiny::h3("Add Data Files"),
  shiny::fluidRow(
    shiny::column(6,
                  shiny::fileInput(inputId = "data_file",
                                   label = "Select App Data (.rds) File",
                                   multiple = FALSE,
                                   accept = ".rds",
                                   buttonLabel = "Browse...",
                                   placeholder = "No file selected")),

    shiny::column(6,
                  # TODO - Can I automatically create one from the rda data above? Or just load a default one. At least with the columns
                  shiny::fileInput(inputId = "dict_file",
                                   label = "Select Dictionary (.csv) File",
                                   multiple = FALSE,
                                   accept = ".csv",
                                   buttonLabel = "Browse...",
                                   placeholder = "No file selected"),
                  # shiny::actionButton(inputId = "default_dict",
                  #                     label = "Create default Dictionary file",
                  #                     width = "100%"),
    ),
  ),



  shiny::fluidRow(
    shiny::column(6,
                  shiny::h4("Columns in Loaded dataframe"),
                  shiny::textOutput(outputId = "data_tbl")),
    shiny::column(6,
                  DT::dataTableOutput(outputId = "data_dict")
                  )
  ),





  # Plotting Overlays -------------------------------------------------------

  shiny::hr(),

  shiny::h3("Plotting Setup"),
  shiny::fluidRow(
    shiny::column(6,
                  shiny::fileInput(inputId = "coast_file",
                                   label = "Load coastline polygon (sf) data",
                                   multiple = FALSE,
                                   accept = ".rds",
                                   buttonLabel = "Browse...",
                                   placeholder = "No file selected")),
    shiny::column(6,
                  shiny::fileInput(inputId = "bndry_file",
                                   label = "Load boundry polygon (sf) data",
                                   multiple = FALSE,
                                   accept = ".rds",
                                   buttonLabel = "Browse...",
                                   placeholder = "No file selected")),
  ),



  #                 shiny::textInput(inputId = "map_theme",
  #                                  label = "Update Map Theme if required",
  #                                  value = "map_theme <- list(
  #                                      ggplot2::theme_bw(),
  #                                      ggplot2::theme(
  #                                        legend.position = "right",
  #                                        legend.direction = "vertical",
  #                                        # text = ggplot2::element_text(size = 6, colour = "black"),
  #                                        axis.text = ggplot2::element_text(size = 9, colour = "black"),
  #                                        plot.title = ggplot2::element_text(size = 12),
  #                                        legend.title = ggplot2::element_text(size = 9),
  #                                        legend.text = ggplot2::element_text(size = 9),
  #                                        axis.title = ggplot2::element_blank()
  #                                      )
  #                                    )"
  #                 )),
  #   shiny::column(6,
  #   ),
  # )

  shiny::hr(),

  shiny::h3("Create App Files"),
  shiny::h5("When you have completed selecting options and uploading files, create the app."),

  shiny::br(),
  shiny::downloadButton(outputId = "save_button",
                        label = "Create shinyplanr Input File",
                        width = "400px"),

  shiny::br(),
  shiny::br(),

  # Put in a footer
  shiny::fluidRow(shiny::column(4, shiny::img(src = "uq-logo-purple.png", width = "90%", style = "float:right; padding: 20px 10px 10px 10px;")), #  align = "right"
                  shiny::column(4, shiny::br(),
                                shiny::HTML("This application was developed by
                                            <a href=http://uq.edu.au target=_blank style='font-weight:bold; color:white; text-decoration: underline'> The University of Queensland</a>,
                                            in conjuction with
                                            <a href = https://www.waittinstitute.org target=_blank style='font-weight:bold; color:white; text-decoration: underline'>The Waitt Institute</a>.
                                            Funding was gratefully provided by a grant from the
                                            <a href = https://www.waittfoundation.org target=_blank style='font-weight:bold; color:white; text-decoration: underline'>The Waitt Foundation</a>."),
                                shiny::br(),
                                shiny::br()),
                  shiny::column(4, shiny::img(src = "WaittRectangleLogo_invert.png", width = "90%", style = "float:left; padding: 10px 10px 10px 10px;")), # align = "right"
                  style = "background-color:#5283B5; color:white",
  )
)

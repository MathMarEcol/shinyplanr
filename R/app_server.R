#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic


  shiny::observe({

    if (options$mod_1welcome == FALSE){
      hideTab(inputId = "navbar", target = "Welcome")
    } else{
      if (shiny::req(input$navbar) == "Welcome"){
        mod_1welcome_server("1welcome_ui_1")
      }
    }

    if (options$mod_2scenario == FALSE){
      hideTab(inputId = "navbar", target = "Scenario")
    } else{
      if (shiny::req(input$navbar) == "Scenario"){
        mod_2scenario_server("2scenario_ui_1")
      }
    }

    if (options$mod_4features == FALSE){
      hideTab(inputId = "navbar", target = "Layer Information")
    } else{
      if (shiny::req(input$navbar) == "Layer Information"){
        mod_4features_server("4features_ui_1")
      }
    }

    if (options$mod_6help == FALSE){
      hideTab(inputId = "navbar", target = "Help")
    } else{
      if (shiny::req(input$navbar) == "Help"){
        mod_6help_server("6help_ui_1")
      }
    }

    if (options$mod_7credit == FALSE){
      hideTab(inputId = "navbar", target = "Credit")
    } else{
      if (shiny::req(input$navbar) == "Credit"){
        mod_7credit_server("7credit_1")
      }
    }

  })

}

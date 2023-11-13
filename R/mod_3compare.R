#' 3compare UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3compare_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 3compare Server Functions
#'
#' @noRd 
mod_3compare_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_3compare_ui("3compare_1")
    
## To be copied in the server
# mod_3compare_server("3compare_1")

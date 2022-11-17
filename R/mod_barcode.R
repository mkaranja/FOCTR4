#' barcode UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_barcode_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' barcode Server Functions
#'
#' @noRd 
mod_barcode_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_barcode_ui("barcode_1")
    
## To be copied in the server
# mod_barcode_server("barcode_1")

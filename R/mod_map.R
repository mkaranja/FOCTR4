#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(class="outer",
        # If not using custom CSS, set height of leafletOutput to a number instead of percent
        leaflet::leafletOutput("map", width="100%", height="100%"),

        # Shiny versions prior to 0.11 should use class = "modal" instead.
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                      width = 330, height = "auto",

                      h2("ZIP explorer"),

                      selectInput("color", "Color", choices = NULL),
                      selectInput("size", "Size", choices = NULL),
                      conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                       # Only prompt for threshold when coloring or sizing by superzip
                                       numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                      ),

                      plotOutput("histCentile", height = 200),
                      plotOutput("scatterCollegeIncome", height = 250)
        ),

        tags$div(id="cite",
                 'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
        )
    )
  )
}

#' map Server Functions
#'
#' @noRd
mod_map_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_map_ui("map_1")

## To be copied in the server
# mod_map_server("map_1")

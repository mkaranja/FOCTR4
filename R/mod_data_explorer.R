#' data_explorer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_explorer_ui <- function(id){
  ns <- NS(id)
  tagList(

    column(12,
        fluidRow(
          panel_div(class_type = "default",
                    content = tags$div(
                    column(4,
                           selectInput(ns("region"), "Region", c(""), multiple=TRUE, width = "100%")
                    )
                    ))
        ),
        hr(),
        fluidRow(
          panel_div(class_type = "default",
                    content = tags$div(
                    DT::DTOutput(ns("table")),
                    conditionalPanel("false", icon("crosshair"))
                    ))
        )
    )
  )

}

#' data_explorer Server Functions
#'
#' @noRd
mod_data_explorer_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      updateSelectInput(session, "region", "Region", c(unique(data$Region)),
                        selected = unique(data$Region))
    })


    filtered_data <- reactive({

      data %>%
          dplyr::filter(Region %in% input$region)

    })


    output$table <- DT::renderDT({
      DT::datatable(filtered_data(),
                    filter = 'top',
                    rownames = FALSE,
                    escape = FALSE,
                    options = list(pageLength = 10,
                                   lengthMenu = c(10, 50, 100, 500,1000),
                                   searchHighlight=T,
                                   stateSave = TRUE)
      )
    })
  })
}

## To be copied in the UI
# mod_data_explorer_ui("data_explorer_1")

## To be copied in the server
# mod_data_explorer_server("data_explorer_1")

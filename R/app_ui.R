#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage(
      title = "FOCTR4",
      inverse = TRUE,
      fluid = T,

      tabPanel(
        title = "INTERACTIVE MAP",
        mod_interactive_map_ui("interactive_map_1")
      ),

      tabPanel(
        title = "DATA EXPLORER",
        mod_data_explorer_ui("data_explorer_1")
      ),

      tabPanel(title = "GENERATE BARCODES",
               mod_generate_barcode_ui("generate_barcode_1")
      )
      )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "FOCTR4"
    ),
    tags$link(href = "www/styles.css", rel = "stylesheet", type = "text/css"),
    tags$link(href = "www/gomap.js", rel = "stylesheet", type = "text/js"),
    #tags$link(href = "www/custom_logging_style.css", rel = "stylesheet", type = "text/css")
    # includeCSS("styles.css"),
    # includeScript("gomap.js"),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    #useShinyjs()
  )
}

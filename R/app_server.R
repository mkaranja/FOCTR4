#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  data <- readRDS("data/foctr4.rds") %>%
    dplyr::mutate(
      cntnt = paste0(
        '<br><strong>Farm Code:</strong> ', `Farm Code`,
        '<br><strong>Region: </strong>',Region,
        '<br><strong>District:</strong> ', District,
        '<br><strong>Farm Size:</strong> ',`Farm Size`)
    )
  mod_interactive_map_server("interactive_map_1", data)
  mod_data_explorer_server("data_explorer_1", data)
  mod_generate_barcode_server("generate_barcode_1")

}

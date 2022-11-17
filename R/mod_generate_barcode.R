#' generate_barcode UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_generate_barcode_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      column(3,
             panel_div(class_type = "default",
                       content = tags$div(
                         shinyWidgets::awesomeRadio(ns("country"), "", choices = c("Tanzania", "Vietnam"),
                                      inline = TRUE, status = "success"),
                         hr(),
                         numericInput(ns("nsamples"), "Number of samples", value=1, min = 1),

                         shinyWidgets::downloadBttn(ns("download"), "Download", style = "material-flat",siz="xs", color = "success")
                       ))
      ),
      column(8,
             panel_div(class_type = "default",
                       content = tags$div(
                         DT::DTOutput(ns("table"))
                       ))
      )
    )
  )
}

#' generate_barcode Server Functions
#'
#' @noRd
mod_generate_barcode_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dataInput <- reactive({
      dt <- dplyr::tbl(conn(), "tbl_sampleIds") %>%
        dplyr::filter(country == input$country) %>%
        dplyr::collect()
      if(nrow(dt)>0){
        current_id <- max(as.integer(str_extract_numbers(dt$sampleId)))
      }else {
        current_id <- 0
      }

      if(input$country == "Tanzania"){
        code <- "TZ"
      } else if(input$country == "Vietnam"){
        code <- "VN"
      }
      df1 <- data.frame(country = input$country, number = input$nsamples)
      df2 <- as.data.frame(df1)[rep(row.names(df1), input$nsamples),] %>%
        tibble::rowid_to_column()
      df2$id <- df2$rowid + current_id
      df2$sampleId <- paste0("IITA_",code,str_pad(df2$id, 5, pad = "0"))
      df2$date <- Sys.Date()
      df2$rowid <- NULL
      df2$number <- NULL
      df2
    })


    output$table <- DT::renderDT({
      req(input$country)
      req(input$nsamples)

      dataInput() %>%
        dplyr::select(-id)
    })

    # on-click, download
    rv <- reactiveValues(download_flag = 0)

    observeEvent(rv$download_flag, {
      confirmSweetAlert(
        session = session,
        inputId = "save_confirm",
        type = "",
        title ="",
        text = "Are you sure you want to download these sample labels?",
        btn_labels = c("Cancel", "Yes, download!"),
        btn_colors = c("#D3D3D3", "#5cb85c")
      )
    }, ignoreInit = TRUE)

    # Create reactiveValues containing confirmation
    save_confirm <- reactiveValues(value = NULL)

    # confirm download
    observeEvent(input$save_confirm, {
      save_confirm$value <- input$save_confirm
      if(save_confirm$value == TRUE){
        db <- dbConnect(SQLite(), sqlitePath)
        dbWriteTable(db, "tbl_sampleIds", dataInput(), append=T)
        #shinyjs::js$refresh()
      }
    }, ignoreInit = TRUE)

    # Download
    downloadInput <- reactive({
      dt <- dataInput()
      dt[rep(seq(nrow(dt)),2),] %>%
        dplyr::arrange(sampleId)
    })
    output$download <- downloadHandler(
      filename = paste(input$country, "Sample ids", Sys.Date(),".pdf"),

      content = function(file) {
        pdf(file, width=8.27, height = 11.69, paper = 'a4', pagecentre=T) # right align width=6.0 # left width=2.0,
        par(mfrow=c(5, 2),mai=c(0.7,0.7,0.7,0.7)#, omi=c(0.5,0.75, 0.5, 0.75)
        ) # right align mar=c(0,30,3,0)

        # pdf(file, width=8.5, height = 11, paper = 'letter', pagecentre=F) # right align width=6.0 # left width=2.0,
        # par(mfrow=c(10, 4),mar=c(1.5,1.5,1.5,1.5), oma=c(1.5,1.5,1.5,1.5)) # right align mar=c(0,30,3,0)
        for(i in 1:(nrow(downloadInput()))){
          image(qrencode_raster(as.character(downloadInput()[i,"sampleId"])), # QRcode
                cex.main = 1.0, cex.sub = 0.8, asp=1, col=c("white", "black"), axes=F,
                xlab="", ylab="",
                subtitle = c(mtext(as.character(downloadInput()[i,"sampleId"]),side = 1, line = 1, outer = F, at = NA, adj = .5, padj = 0, cex = 1, col = 1, las=1, font = 10),
                             mtext(as.character(downloadInput()[i,"sampleId"]),side = 3, line = 1, outer = F, at = NA, adj = .5, padj = 0, cex = 1, col = 1, las=1, font = 10))
          )

        }
        dev.off()
        rv$download_flag <- rv$download_flag + 1
      }
    )
  })
}

## To be copied in the UI
# mod_generate_barcode_ui("generate_barcode_1")

## To be copied in the server
# mod_generate_barcode_server("generate_barcode_1")

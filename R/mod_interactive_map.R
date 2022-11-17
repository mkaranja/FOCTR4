#' interactive_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_interactive_map_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(class="outer",
        # If not using custom CSS, set height of leafletOutput to a number instead of percent
        leaflet::leafletOutput(ns("map"), width="100%", height="100%"),

        # Shiny versions prior to 0.11 should use class = "modal" instead.
        absolutePanel(id = ns("controls"), class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                      width = 330, height = "auto",

                      panel_div(class_type = "default",
                                content = tags$div(
                                h2("Explorer"),

                                selectInput(ns("color"), "Color",
                                            choices = c("Gender", "Education", "Variety", "Yellowing","Fusarium/ Panama"))
                                ))
        )
        # tags$div(id="cite",
        #          'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
        # )
    )
  )
}

#' interactive_map Server Functions
#'
#' @noRd
mod_interactive_map_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$map <- leaflet::renderLeaflet({

      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::setView(lng = 37.87614, lat = -7.561166, zoom = 7) %>%
        leaflet::addMarkers(
          data = data,
          lat =  ~Latitude,
          lng =~Longitude,
          popup = ~as.character(cntnt),
          group = input$color,
          #label = "Default Label",
          labelOptions = leaflet::labelOptions(noHide = T))
    })
    # output$map <- leaflet::renderLeaflet({
    #   colorBy <- "Age of Plantation" #input$color
    #
    #   leaflet::leaflet(data) %>%
    #     leaflet::addCircles(lng = ~Longitude, lat = ~Latitude) %>%
    #     leaflet::addTiles() %>%
    #     leaflet::addCircleMarkers(data = data, lat =  ~Latitude, lng =~Longitude,
    #                      radius = 3, popup = ~as.character(cntnt),
    #                      color = ~pal(colorBy),
    #                      stroke = FALSE, fillOpacity = 0.8)%>%
    #     leaflet::addLegend(pal=pal, values=data[[colorBy]],opacity=1, na.label = "Not Available")#%>%
    #     # leaflet::addEasyButton(
    #     #   leaflet::easyButton(
    #     #   icon="fa-crosshairs", title="ME",
    #     #   onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    # })

    # # # Create the map
    # output$map <- leaflet::renderLeaflet({
    #   leaflet::leaflet() %>%
    #     leaflet::addTiles() %>%
    #     leaflet::setView(lng = 37.87614, lat = -7.561166, zoom = 7)
    # })


    # A reactive expression that returns the set of farms that are
    # in bounds right now
    # inBounds <- reactive({
    #   if (is.null(input$map_bounds))
    #     return(data[FALSE,])
    #   bounds <- input$map_bounds
    #   latRng <- range(bounds$north, bounds$south)
    #   lngRng <- range(bounds$east, bounds$west)
    #
    #   subset(data,
    #          Latitude >= latRng[1] & Latitude <= latRng[2] &
    #            Longitude >= lngRng[1] & Longitude <= lngRng[2])
    # })
    #
    #
    # # # This observer is responsible for maintaining the circles and legend,
    # # # according to the variables the user has chosen to map to color and size.
    # observe({
    #   colorBy <- input$color
    #   sizeBy <- input$size
    #
    #     colorData <- data[[colorBy]]
    #     pal <- leaflet::colorBin("viridis", colorData, 7, pretty = FALSE)
    #
    #     radius <- data[[sizeBy]] / max(data[[sizeBy]]) * 30000
    #
    #
    #     leaflet::leafletProxy("map", data = data) %>%
    #       leaflet::clearShapes() %>%
    #       leaflet::addCircles(~Longitude, ~Latitude, radius=radius, layerId=~SampleNo,
    #                stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
    #       leaflet::addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
    #               layerId="colorLegend")
    # })

    # # Show a popup at the given location
    # # showPopUp <- function(SampleNo, lat, lng) {
    # #   selectedZip <- data[data$SampleNo == SampleNo,]
    # #   content <- as.character(tagList(
    # #     tags$h4("Score:", as.integer(selectedZip$centile)),
    # #     tags$strong(HTML(sprintf("%s, %s %s",
    # #                              selectedZip$city.x, selectedZip$state.x, selectedZip$SampleNo
    # #     ))), tags$br(),
    # #     sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
    # #     sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
    # #     sprintf("Adult population: %s", selectedZip$adultpop)
    # #   ))
    # #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = SampleNo)
    # # }
    #
    # # When map is clicked, show a popup with city info
    # observe({
    #   leaflet::leafletProxy("map") #%>% clearPopups()
    #   event <- input$map_shape_click
    #   if (is.null(event))
    #     return()
    #
    #   # isolate({
    #   #   showPopUp(event$id, event$lat, event$lng)
    #   # })
    # })

  })
}

## To be copied in the UI
# mod_interactive_map_ui("interactive_map_1")

## To be copied in the server
# mod_interactive_map_server("interactive_map_1")

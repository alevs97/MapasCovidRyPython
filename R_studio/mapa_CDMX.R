library(shiny)
library(leaflet)
library(RColorBrewer)

data1 <- read.csv("../map_docs/mapa_CDMX_total_de_casos.csv")


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Numero de casos", min(data1$cases), max(data1$cases),
                            value = range(data1$cases), step = 0.1
                ),
                selectInput("colors", "Color del esquema",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Rangos IDH", TRUE),
                
                
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    data1[data1$cases>= input$range[1] & data1$cases <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    #(input$colors,data$TOP )
    colorQuantile(input$colors, data1$cases)
  })
  
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(data1) %>% addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()

    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~sqrt(10000)*30, stroke = FALSE,
                 fillColor = ~pal(cases),fillOpacity = 0.8,popup = ~paste(alcaldias,cases,"Casos presentados",sep=": ")
      )%>%
      setView(lng=-99.12766, lat=19.42847 , zoom=10)
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = data1)
    #IDH <- data[ ,input$Total_Casos]
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~data1$cases
      )
    }
  })
  gc()
  memory.size(max=F)
}

shinyApp(ui, server)

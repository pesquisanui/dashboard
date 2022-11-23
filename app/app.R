
library(shiny)
#library(shinycssloaders)
library(shinybusy)
#library(htmlwidgets)

library(dplyr)
library(tibble)
library(sf)

library(leaflet)
library(DT)

### UI ------------------------------------
ui <- fillPage(
  
  leafletOutput("mapa1", width = "100%", height = "100%"),
  
  fixedPanel(
    bottom = 60, 
    right = 10, 
#    draggable = TRUE, 
    width = "60vmin",
#    height = "40vmin",
    style = "background: white; border-style: solid; border-width: 2px;", # border-color: #10607E;
    h5(HTML("<b>Características do Núcleo Urbano Informal</b>"),
       style="padding-left: 10px; padding-right: 10px;"),
    DTOutput("tabela1", width = "100%", height = "100%")
  ),

  add_busy_spinner(spin = "fading-circle")
)

### SERVER ------------------------------------
server <- function(input, output) {
  
  # open data
  muns <- readRDS("muns.rds")
  nuis1 <- readRDS("nuis1.rds")
  nuis2 <- readRDS("nuis2.rds")

  # define reactive
  p1 <- reactive({
    input$mapa1_shape_click
  })
  
  # mapa1
  output$mapa1 <- renderLeaflet({
    
    leaflet() %>%
      setView(lng = -40, lat = -20, zoom = 4) %>%
      addProviderTiles(
        providers$Esri.WorldImagery, 
        group = "Satélite",
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addProviderTiles(
        providers$OpenStreetMap.Mapnik, 
        group = "Padrão",
      ) %>%
      addLayersControl(
        baseGroups = c(
          "Satélite",
          "Padrão"
        ),
        options = layersControlOptions(
          collapsed = TRUE
        )
      ) %>%
      addPolygons(
        data = muns,
        col = "black",
        weight = 2,
        fillColor = "transparent"
      ) %>%
      addPolygons(
        data = nuis1,
        color = "red",
        weight = 2,
#        opacity = 1,
        fillColor = "transparent",
        highlight = highlightOptions(color = "#10607E", weight = 3),
        layerId = ~V0
      )
    
  })
  
  observeEvent(input$mapa1_shape_click, {
    
    if (!is.null(input$mapa1_shape_click$id)){
      leafletProxy("mapa1") %>% 
        addPolygons(data = nuis1 %>% filter(V0 == input$mapa1_shape_click$id), 
                    color = "#10607E",
                    opacity = 1,
                    weight = 3,
                    fillColor = "transparent",
                    options = pathOptions(clickable = FALSE),
                    layerId = "foo")
    }
    
    else {
      leafletProxy("mapa1") %>%
        removeShape(layerId = "foo")
    }
    
  })
  
  # tabela1
  output$tabela1 <- renderDT({
    
    if (!is.null(input$mapa1_shape_click$id)){
      tabela1 <- tibble(
        "Variável" = names(nuis2),
        "Descrição" = nuis2 %>% filter(`Identificador do NUI` == input$mapa1_shape_click$id) %>% t()
      )

    }
    
    else {
      tabela1 <- tibble(
        "Variável" = names(nuis2),
        "Descrição" = ""
      )
    }
    
    datatable(tabela1,
              rownames = FALSE,
              options = list(#scrollX = TRUE,
                             scrollY = "30vh",
                             pageLength = nrow(tabela1),
                             dom = 't',
                             ordering = F,
                             columnDefs = list(
                               list(width = '20%', targets = 0),
                               list(width = '80%', targets = 1)),
                             initComplete = DT::JS(
                               "function(settings, json) {",
                               "$('th').css({'background-color': '#10607E'});",
                               "$('th').css({'color': 'white'});",
                               "}")
                             ),
              selection = list(mode = "none"))
  })

}

### APP ------------------------------------
shinyApp(ui = ui, server = server)

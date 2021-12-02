# MAPA 3 - SUPERFICIES DE PROBABILIDADE

token <- "pk.eyJ1IjoibHVpc2ZlbGlwZWJyIiwiYSI6ImNrbno1MW0wbTAyMHAybm8zcGkxeW9neWYifQ.SgfmjARAewAGdn9MZbahYQ"

# create variable polo de pesquisa
variavel3 <- reactiveValues()

# observe polo de pesquisa
observeEvent({input$polo3}, {
  
  if(input$polo3 != "") {
    variavel3$polo <- input$polo3
  } else {
    variavel3$polo <- NULL
  }
  print(variavel3$polo)
  
})

# observe basemap layer
observeEvent({input$layer3}, {

  if(input$layer3 == "Satélite") {
    variavel3$layer <- "satellite"
  } else {
    variavel3$layer <- "light"
  }
  print(variavel3$layer)
  
})

output$mapeamento3 <- renderUI({
  if (is.null(variavel3$polo)) return()
  radioButtons(
    inputId = "mapeamento3",
    label = ("Mapeamento:"),
    choices = c("Núcleos Urbanos Informais (NUI)", "Aglomerados Subnormais (AGSN)"),
    selected = "Núcleos Urbanos Informais (NUI)"
  )
})

output$probabilidade3 <- renderUI({
  if (is.null(variavel3$polo)) return()
  radioButtons(
    inputId = "probabilidade3",
    label = ("Superfície de probabilidade:"),
    choices = c("Y = AGSN", "Y = NUI"),
    selected = "Y = NUI"
  )
})

# create basemap
output$mapa3 <- renderMapdeck({

  mapdeck(token = token,
          style = mapdeck_style("light"),
          location = c(-43.95988, -19.902739), 
          zoom = 3)
    
})

# update polo de pesquisa
observeEvent({variavel3$polo},{
  
  tabf <- tab %>% filter(Polo == variavel3$polo)
  
  mun <- read_rds("dados/appmun.rds") %>% filter(UF == tabf$UF)
  nui <- read_rds("dados/appnui.rds") %>% filter(UF == tabf$UF)
  prob <- read_rds("dados/appprob.rds") %>% filter(UF == tabf$UF)
  
  mapdeck_update(map_id = "mapa3") %>%
    # zoom to polo de pesquisa
    mapdeck_view(
      location = c(tabf$lng, tabf$lat),
      zoom = 10
    ) %>%
    # clear layers
    clear_polygon(layer_id = "mun") %>%
    clear_polygon(layer_id = "nui") %>%
    clear_polygon(layer_id = "prob") %>%
    clear_legend(layer_id = "prob") %>%
    # add layers
    
    add_polygon(
      update_view = FALSE,
      data = prob,
      layer_id = "prob",
      fill_colour = "NUI",
      #tooltip = "NUI",
      fill_opacity = 0.5,
      stroke_opacity = 1,
      legend = list(
        fill_colour = TRUE,
        stroke_colour = FALSE
      ),
      legend_options = list(
        fill_colour = list(title = "Probabilidade")
      )
    ) %>%
    add_polygon(
      update_view = FALSE,
      data = nui,
      layer_id = "nui",
      fill_opacity = 1,
      stroke_colour = "#000000",
      stroke_width = 25,
      legend = FALSE
    ) %>%
    add_polygon(
      update_view = FALSE,
      data = mun,
      layer_id = "mun",
      fill_opacity = 1,
      stroke_colour = "#000000",
      stroke_width = 100,
      legend = FALSE
    )
    
    

  
})

# update basemap
observeEvent({variavel3$layer},{
  
  mapdeck_update(map_id = "mapa3") %>%
    update_style(style = mapdeck_style(variavel3$layer))
  
})




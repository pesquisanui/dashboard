# MAPA 2 - DADOS SECUNDARIOS

observeEvent({input$variavel2}, {
  
  tabf <- filter(tab, Polo == input$variavel2)
  
  mun <- read_rds("dados/appmun.rds") %>% filter(UF == tabf$UF)
  
  output$mapa2 <- renderMapdeck({
    
    mapdeck(
      style = mapdeck_style("light")
    )
    
  })
  
})

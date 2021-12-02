# CONTROLE 1 - LEVANTAMENTO DE CAMPO

output$controle1 <- renderUI({
  
      selectInput(
        inputId = "variavel1",
        label = ("Selecione o polo de pesquisa:"),
        choices = tab$Polo
      )
  
})
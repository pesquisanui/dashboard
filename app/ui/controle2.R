# CONTROLE 2 - DADOS SECUNDARIOS

output$controle2 <- renderUI({
  
  selectInput(
    inputId = "variavel2",
    label = ("Selecione o polo de pesquisa:"),
    choices = tab$Polo
  )
  
})
# MAPA 1 - LEVANTAMENTO DE CAMPO

observeEvent({input$variavel1}, {
  
  tabf <- filter(tab, Polo == input$variavel1)
  
  nui <- read_rds("dados/appnui.rds") %>% filter(UF == tabf$UF)
  mun <- read_rds("dados/appmun.rds") %>% filter(UF == tabf$UF)
  
  p1 <- reactive({
    input$mapa1_shape_click
  })
  
  output$mapa1 <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(
        providers$Esri.WorldImagery, 
        group = "Satélite (Esri)",
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addProviderTiles(
        providers$Esri.WorldTopoMap, 
        group = "Padrão (Esri)",
      ) %>%
      addLayersControl(
        baseGroups = c(
          "Satélite (Esri)",
          "Padrão (Esri)"
        ),
        options = layersControlOptions(
          collapsed = TRUE
        )
      ) %>%
      addPolygons(
        data = mun,
        col = "black",
        weight = 2,
        fillColor = "transparent"
      ) %>%
      addPolygons(
        data = nui,
        color = "white",
        weight = 2,
        fillColor = "transparent",
      ) %>%
      addPolygons(
        data = nui,
        color = "red",
        weight = 3,
        fillColor = "transparent",
        highlight = highlightOptions(color = "yellow", weight = 3),
        layerId = ~V0
      ) %>%
      setView(
        lng = tabf$lng,
        lat = tabf$lat,
        zoom = 10
      ) %>%
      setMaxBounds(
        lng1 = tabf$lng1,
        lng2 = tabf$lng2,
        lat1 = tabf$lat1,
        lat2 = tabf$lat2
      )
    
  })
  
  
  
  output$tabela1 <- renderDT({
    
    p1 <- input$mapa1_shape_click
    
    if (!is.null(p1$id)){
      tabela1 <- nui %>% filter(V0 == p1$id) %>% st_drop_geometry() %>%
        mutate(V5a = case_when(V5a == "1" ~ "Favela ou ocupação espontânea",
                               V5a == "2" ~ "Loteamento irregular ou clandestino",
                               V5a == "3" ~ "Conjunto Habitacional",
                               V5a == "4" ~ "Distrito ou povoado",
                               V5a == "5" ~ "Sede Municipal",
                               V5a == "6" ~ "Ocupação por populações tradicionais",
                               V5a == "7" ~ "Outro"),
               V6 = case_when(V6 == "1" ~ "Menos de 1 ano",
                              V6 == "2" ~ "De 1 a 5 anos",
                              V6 == "3" ~ "De 5 a 10 anos",
                              V6 == "4" ~ "Acima de 10 anos",
                              V6 == "5" ~ "Não tem informação",
                              V6 == "-" ~ "Não tem informação"),
               V7 = case_when(V7 == "1" ~ "Rápido surgimento de novas moradias",
                              V7 == "2" ~ "Lento surgimento de novas moradias",
                              V7 == "3" ~ "Estável",
                              V7 == "4" ~ "Lenta diminuição do número de moradias",
                              V7 == "5" ~ "Rápida diminuição do número de moradias",
                              V7 == "6" ~ "Não foi possível aferir"),
               V8 = case_when(V8 == "1" ~ "Completamente isolado da malha urbana da cidade",
                              V8 == "2" ~ "Na periferia da malha urbana da cidade",
                              V8 == "3" ~ "Completamente inserido na malha central urbana da cidade",
                              V8 == "4" ~ "Não foi possível aferir"),
               V9 = case_when(V9 == "1" ~ "Sim",
                              V9 == "2" ~ "Não",
                              V9 == "3" ~ "Parcialmente",
                              V9 == "4" ~ "Não se aplica"),
               V10a = case_when(V10a == "1" ~ "Não",
                                V10a == "2" ~ "Sim - Unidade de Proteção Integral",
                                V10a == "3" ~ "Sim - Unidade de Uso Sustentável",
                                V10a == "4" ~ "Sim - Área de Proteção aos Mananciais",
                                V10a == "5" ~ "Sim - Outras Áreas Protegidas"),
               V11a = case_when(V11a == "1" ~ "Não tem informação",
                                V11a == "2" ~ "Não",
                                V11a == "3" ~ "Sim: APP hídrica - identificada por imagem de satélite",
                                V11a == "4" ~ "Sim: APP hídrica - indicada pela prefeitura",
                                V11a == "5" ~ "Sim: APP hídrica - outra fonte",
                                V11a == "6" ~ "Sim: outra APP"),
               V12a = case_when(V12a == "1" ~ "Não tem informação",
                                V12a == "2" ~ "Não",
                                V12a == "3" ~ "Sim (especificar na coluna ao lado)"),
               V13a = case_when(V13a == "1" ~ "Não tem informação",
                                V13a == "2" ~ "Não",
                                V13a == "3" ~ "Completamente",
                                V13a == "4" ~ "Parcialmente"),
               V14a = case_when(V14a == "1" ~ "Traçado com padrão semelhante ao da cidade formal \n(vias veiculares adequadas que estruturam a ocupação, \ntodos os lotes têm acesso direto à rua)",
                                V14a == "2" ~ "Predomina traçado regulador (vias veiculares \nestruturam o NUI, mas nem todas são adequadas e/ou \nalguns lotes não têm acesso direto à rua)",
                                V14a == "3" ~ "Predomina ausência de traçado regulador (poucas quadras \nsão estruturadas por vias veiculares, a maioria dos lotes \né acessado por meio de vias estreitas, vielas ou escadarias)",
                                V14a == "4" ~ "Ausência de traçado regulador (acesso ao interior do NUI \nfeito exclusivamente por vielas de pedestres, becos ou escadarias)",
                                V14a == "5" ~ "Outro"),
               V15a = case_when(V15a == "1" ~ "Predominam lotes bem definidos e existe distanciamento \nentre as construções (recuos e espaços entre as habitações)",
                                V15a == "2" ~ "Predominam lotes bem definidos e pouco ou nenhum \ndistanciamento entre as construções (recuos e espaços entre as habitações)",
                                V15a == "3" ~ "Predominam lotes indefinidos, mas existe distanciamento \nentre as construções (recuos e espaços entre as habitações)",
                                V15a == "4" ~ "Predominam lotes indefinidos com alta ocupação \n(difícil identificar divisão entre os lotes e as casas)",
                                V15a == "5" ~ "Outras"),
               V16a = case_when(V16a == "1" ~ "Predominam habitações de padrão popular \n(consolidadas e com dimensões e qualidade aceitáveis)",
                                V16a == "2" ~ "Predominam habitações em diferentes estágios \nde consolidação e precariedade (padrão popular, \nmateriais improvisados, construções precárias, etc.)",
                                V16a == "3" ~ "Predominam habitações precárias e/ou \nimprovisadas",
                                V16a == "4" ~ "Outras",
                                V16a == "5" ~ "Não foi possível aferir"),
               V17 = case_when(V17 == "1" ~ "Possui infraestrutura adequada",
                               V17 == "2" ~ "Possui infraestrutura parcial",
                               V17 == "3" ~ "Ausência de infraestrutura básica ou \ninfraestrutura existente muito precária",
                               V17 == "4" ~ "Não foi possível aferir"))
      
      tabela1 <- as_tibble(cbind(nms = names(tabela1), t(tabela1))) %>% rename(codigo = nms)
      
      tabela1 <- relat %>% left_join(tabela1) %>% select(-codigo) %>% rename("Variável" = "variavel", "Descrição" = "1")
    }
    
    else {
      tabela1 <- relat %>% mutate("1" = "") %>% select(-codigo) %>% rename("Variável" = "variavel", "Descrição" = "1")
    }
    
    datatable(tabela1,
              rownames = FALSE,
              options = list(scrollX = TRUE,
                             scrollY = "500px",
                             pageLength = nrow(tabela1),
                             dom = 't',
                             ordering = F),
              selection = list(mode = "none"))
  })
  
})

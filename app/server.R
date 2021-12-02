
library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(sf)
library(leaflet)
library(DT)
library(mapdeck)

token <- "pk.eyJ1IjoibHVpc2ZlbGlwZWJyIiwiYSI6ImNrbno1MW0wbTAyMHAybm8zcGkxeW9neWYifQ.SgfmjARAewAGdn9MZbahYQ"

tab <- tibble(Polo = c("", "Belo Horizonte - MG", "Brasília - DF", "Juazeiro do Norte - CE", "Marabá - PA", "Recife - PE", "Porto Alegre - RS"),
              UF = c(NA, "MG", "DF", "CE", "PA", "PE", "RS"),
              lng = c(NA, -43.94, -47.87, -39.32, -49.1, -35.00, -51.23),
              lat = c(NA, -19.92, -15.79, -7.21, -5.35, -8.05, -30.03),
              lng1 = c(NA, -43, -46, -38, -47, -34, -50),
              lng2 = c(NA, -45, -49, -41, -52, -36, -52),
              lat1 = c(NA, -19, -14, -6, -4, -7, -29),
              lat2 = c(NA, -21, -18, -8, -7, -9, -32))

relat <- structure(list(codigo = c("V2", "V3a", "V4a", "V4b", "V4c", "V5a", 
                                   "V6", "V7", "V8", "V14a", "V15a", "V16a", "V17", "V9", "V10a", 
                                   "V11a", "V12a", "V13a"), 
                        variavel = c("Município", "Nome do NUI", 
                                     "Quantidade de domicílios", "Fonte", "Data", "Tipo", "Tempo de estabelecimento", 
                                     "Dinâmica imobiliária", "Contiguidade urbana", "Traçado (vias e acesso aos lotes)", 
                                     "Ocupação e definição dos lotes", "Condição das construções", 
                                     "Urbanização e infraestrutura", "O NUI ou parte dele insere-se em ZEIS?", 
                                     "O NUI ou parte dele insere-se em UC, APM, ou outras áreas protegidas?", 
                                     "Existe APP no interior do NUI?", "Existe indício de situação de risco no interior do NUI?", 
                                     "O NUI insere-se em área de suscetibilidade média ou alta a risco?")), 
                   row.names = c(NA, -18L), 
                   class = c("tbl_df", "tbl", "data.frame"))

server <- function(input, output, session) {
  
  source("ui/controle1.R", local = TRUE)
  source("server/mapa1.R", local = TRUE)

  source("ui/controle2.R", local = TRUE)
  source("server/mapa2.R", local = TRUE)
  
  source("ui/controle3.R", local = TRUE)
  source("server/mapa3.R", local = TRUE)
  
}

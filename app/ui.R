
library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(sf)
library(leaflet)
library(DT)
library(mapdeck)

library(htmltools)
library(shinyjs)
library(shinycssloaders)

token <- "pk.eyJ1IjoibHVpc2ZlbGlwZWJyIiwiYSI6ImNrbno1MW0wbTAyMHAybm8zcGkxeW9neWYifQ.SgfmjARAewAGdn9MZbahYQ"
mapdeck::set_token(token = token)

tab <- tibble(Polo = c("", "Belo Horizonte - MG", "Brasília - DF", "Juazeiro do Norte - CE", "Marabá - PA", "Recife - PE", "Porto Alegre - RS"),
              UF = c(NA, "MG", "DF", "CE", "PA", "PE", "RS"),
              lat = c(NA, -43.94, -47.87, -39.32, -49.1, -35.00, -51.23),
              lon = c(NA, -19.92, -15.79, -7.21, -5.35, -8.05, -30.03))

ui <- fluidPage(
  
  fluidRow(
    
    tabBox(
      
      width = 12,
      
      tabPanel("Levantamento de Campo",
               fluidRow(
                 column(
                   12,
                   uiOutput(
                     'controle1'
                   )
                 )
               ),
               fluidRow(
                 column(
                   6,
                   leafletOutput(
                     "mapa1",
                     height = "550px"
                     ) %>%
                     withSpinner()
                 ),
                 column(
                   6,
                   DTOutput(
                     "tabela1",
                     height = "550px"
                     )
                   )
                 )
               
      ),
      
      tabPanel("Dados secundários",
               fluidRow(
                 column(
                   12,
                   uiOutput(
                     'controle2'
                   )
                 )
               ),
               fluidRow(
                 column(
                   4,
                   selectInput(
                     inputId = "polo2",
                     label = ("Polo de pesquisa:"),
                     choices = tab$Polo
                   )
                 ),
                 column(
                   8,
                   mapdeckOutput(
                     "mapa2",
                     height = "550px"
                   )
                 )
                )
               
      ),
      
      tabPanel("Superfícies de Probabilidade",
               fluidRow(
                 column(
                   4,
                   selectInput(
                     inputId = "polo3",
                     label = ("Polo de pesquisa:"),
                     choices = tab$Polo
                   ),
                   uiOutput("mapeamento3"),
                   uiOutput("probabilidade3"),
                   radioButtons(
                     inputId = "layer3",
                     label = ("Mapa base:"),
                     choices = c("Padrão", "Satélite"),
                     selected = "Padrão"
                   ),
                 column(
                   8,
                   mapdeckOutput(
                     "mapa3",
                     height = "550px"
                   )
                  )
                 )
                 )
      )
      
    )
    
  )
  
)
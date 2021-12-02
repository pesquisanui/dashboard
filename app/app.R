
library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(sf)
library(leaflet)
library(DT)
library(mapdeck)

token <- "pk.eyJ1IjoibHVpc2ZlbGlwZWJyIiwiYSI6ImNrbno1MW0wbTAyMHAybm8zcGkxeW9neWYifQ.SgfmjARAewAGdn9MZbahYQ"
mapdeck::set_token(token = token)

source("app/ui.r")
source("app/server.r")

shinyApp(ui = ui, server = server)

rsconnect::setAccountInfo(name='luisfelipebc', 
                          token='41AB29675173CA3356A46CD9B2E99B94', 
                          secret='YYTD4kebAlGibi5r9nLUs2edCLh1HoH7YYgLw+vb')

rsconnect::deployApp(appName = "painel")

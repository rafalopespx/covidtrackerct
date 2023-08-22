### Loading necessary libraries  ####
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(shinyjs)
library(shinyauthr)
library(plotly)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(xgboost)

### Calling the child-codes ####
source("auxiliar_dasg.R", encoding = "UTF-8")
source("my_server.R", encoding = "UTF-8")
source("my_ui.R", encoding = "UTF-8")

### Integrate ####
shinyApp(
    ui = ui, 
    server = server
)
  
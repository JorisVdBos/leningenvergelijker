# Libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinysky)

library(data.table)
library(lubridate)
library(DT)
library(feather)
library(rCharts)

# Functies
for (script in list.files("functies")) {
  source(file.path("functies", script))
}

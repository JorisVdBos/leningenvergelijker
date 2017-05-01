# Libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinysky)

library(data.table)
library(DT)
library(feather)

# Functies
for (script in list.files("functies")) {
  source(file.path("functies", script))
}

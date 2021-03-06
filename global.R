# Libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinysky)

library(data.table)
library(lubridate)
library(DT)
library(feather)
# library(rCharts)
library(ggplot2)
library(scales)

# Functies
for (script in list.files("functies")) {
  source(file.path("functies", script))
}


# Parameters en opties
metrieken <- c('Totaal_Afbetalingen', 'Totaal_Interesten', 'Totaal_Extra_Kosten', 
               'Vermogen_EindeLening', 
               'Vermogen_Belegging_Opbrengsten')
options(scipen = 999)


# Devtools
testing <- FALSE
opslaanOpgeslagenLeningen <- TRUE

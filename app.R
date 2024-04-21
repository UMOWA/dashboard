# Author: Melissa L Muradian
# https://umowa.shinyapps.io/dashboard/
# Alright, this is dashboard version 3, written May 10, 2023
# Greatly slimmed down and with the idea of pushing the complete set
# of exploratory figures to another sight altogether
# Last updated April 2024 - MLM

library(leaflet)
library(shiny)
library(shinydashboard)
library(shinythemes)
# library(graphics)
library(png)
# library(plotrix) # for plotCI()

# source("www/doughnut.R", local=T)

source('global.R', local = TRUE)
source('umowaUI.R', local = TRUE)
source('umowaServer.R', local = TRUE)
########################################################################

shinyApp(
  ui = ui,
  server = server
)



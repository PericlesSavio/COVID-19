#packages
library("shiny")
install.packages("shinydashboard")
library("shinydashboard")
library("dplyr") # manipular dataframes
library("maps") # funcao mapa()
library("mapdata") # funcao mapa()
library("leaflet") # mapa interativo
#library(DT) #?
#library("htmlwidgets")

#shiny
source("covid.R")
source("ui.R")
source("server.R")


# Run the application 
shinyApp(ui, server)

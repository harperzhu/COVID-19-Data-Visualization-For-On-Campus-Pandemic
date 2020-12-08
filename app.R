# #loading the necessary libraries and packages
library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
source("ui.R")
source("server.R")

# # Calling the other files
# shinyApp(ui = ui, server = server)
# 
if (interactive()) {
        
        library(shiny)
        library(shinyWidgets)
        
        ui <- fluidPage(
                tags$h2("Change shiny app background"),
                setBackgroundColor(
                        color = "ghostwhite",
                        gradient = c("linear", "radial"),
                        direction = c("bottom", "top", "right", "left"),
                        shinydashboard = FALSE
                )
        )
        
        server <- function(input, output, session) {}
        
        shinyApp(ui, server)
        
}
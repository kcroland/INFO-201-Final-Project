
library(shiny)
source("server.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Pollution Concetration and Bee Colonies"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("pollutant", label = h3("Pollutant"), 
                  choices = list("Nitrogen dioxide" = "NO2", "Ozone" = "O3",
                                 "Sulfur dioxide" = "SO2",
                                 "Carbon monoxide" = "CO"),
                  selected = "O3"),
      
      selectInput("select_state", label = h3("Select State"), 
                  choices = uni_names, 
                  selected = "California")
    ),
    mainPanel(
       plotOutput("pollution_vs_colonies"),
       plotOutput("all_states")
    )
  )
))

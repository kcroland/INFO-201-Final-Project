## load shiny
library("shiny")
library("ggplot2")
library("ggmap")
library("maps")
library("mapdata")
library("dplyr")

# Reading vHoneyNeonic
neonic <- read.csv("data/vHoneyNeonic_v03.csv", header = TRUE, sep = ",")
source('process.R')
my_server <- function(input, output) {
  
  ## Generate Static Dropdown Select
  states_list <- sort(unique(data.frame(lapply(neonic, as.character), stringsAsFactors=FALSE)[[9]]))
  area_selector_options <- append(list("All States"), states_list)
  output$state_selector_xy <- renderUI({
    selectInput('state_xy', 'Filter By State', area_selector_options, "All States")
  })
  
  # x dropdown
  xlist <- list('Year', 'Honey Yield', 'Amount of Neonic Pesticides')
  output$x_xy_selector <- renderUI({
    selection <- xlist
    if(!is.null(input$y_xy) && !is.na(input$y_xy)) {
      selection <- xlist[sapply(xlist, function(x) x!=input$y_xy)]
    }
    if(!is.null(input$x_xy) && !is.na(input$x_xy)) {
      return(selectInput('x_xy', 'x axis', selection, input$x_xy))
    }
    return(selectInput('x_xy', 'x axis', selection))
  })
  
  # y dropdown
  ylist <- list('Honey Yield', 'Year', 'Amount of Neonic Pesticides')
  output$y_xy_selector <- renderUI({
    selection <- ylist
    if(!is.null(input$x_xy) && !is.na(input$x_xy)) {
      selection <- ylist[sapply(ylist, function(y) y!=input$x_xy)]
    }
    if(!is.null(input$y_xy) && !is.na(input$y_xy)) {
      return(selectInput('y_xy', 'y axis', selection, input$y_xy))
    }
    return(selectInput('y_xy', 'y axis', selection))
  })
  
  output$plot_xy <- renderPlot({
    data_mich <- by_year(neonic)
    x <- "Year"
    y <- "Honey Yield"
    if (!is.null(input$x_xy) && !is.na(input$x_xy)) {
      x <- input$x_xy
    }
    if (!is.null(input$y_xy) && !is.na(input$y_xy)) {
      y <- input$y_xy
    }
    if (!is.null(input$state_xy) && !is.na(input$state_xy) && input$state_xy != "All States") {
      data_mich <- filter_by_state(input$state_xy, neonic)
    }
    
    if(!is.null(input$x_xy) &&
       !is.na(input$x_xy) &&
       !is.null(input$y_xy) &&
       !is.na(input$y_xy) &&
       (input$x_xy == 'Year' || input$y_xy == 'Year')
       ) {
      data_mich <- by_year(data_mich)
    }
    return(plot_var(data_mich, name_to_col(data_mich, x), name_to_col(data_mich, y), paste0(y, " by ", x, " for ", input$state_xy), x, y))
  })
}


shinyServer(my_server)

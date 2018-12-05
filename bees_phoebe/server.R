#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)

             
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  data <- read.csv("vHoneyNeonic_v03.csv", stringsAsFactors = FALSE)
  uni_names <- as.list(unique(data$StateName))
  
  output$plot <- renderPlot({
    if (input$radio == "Region") {
       reg_or_state <- input$radio
       colonies_per_year <- data %>% 
                            group_by(year, Region) %>% 
                            summarise(col_total = sum(numcol, na.rm = TRUE)) 
       ggplot(colonies_per_year, aes(x = year, y = col_total, group = Region, color = Region)) +
         geom_point(size = 1) + geom_line() +
         labs(title = "Total Number of Honeybee Colonies by Region",
              x = "Year",
              y = "Number of Colonies",
              color = "Region") +
         scale_x_continuous(breaks = seq(1991, 2017, by = 2))
    } else {
       state_name <- input$select
       colonies_per_year <- data %>% 
                            filter(StateName == state_name) %>% 
                            select(numcol, year)
       ggplot(colonies_per_year, aes(x = year, y = numcol)) +
         geom_point(size = 2, col = "#f4be41") + geom_line(size = 1, col = "#f4c741") + 
         labs(title = paste0("Total Number of Honeybee Colonies in ", state_name),
              x = "Year",
              y = "Number of Colonies") +
         scale_x_continuous(breaks = seq(1991, 2017, by = 2))
    }
  })
  
})

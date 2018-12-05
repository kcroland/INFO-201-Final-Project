
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

bee_data <- read.csv("data/vHoneyNeonic_v03.csv")
uni_names <- as.list(unique(bee_data$StateName))
# pollution_data <- read.csv("data/pollution_us_2000_2016.csv")

# pollution_data <- pollution_data %>%
#                   select(State, Date.Local, NO2.Units, NO2.Mean,
#                          O3.Units, O3.Mean, SO2.Units, SO2.Mean,
#                          CO.Units, CO.Mean)

# pollution_data$Date.Local <- as.Date(pollution_data$Date.Local)
# pollution_data$Date.Local <- as.numeric(format(pollution_data$Date.Local, '%Y'))
# 
# pollution_means <- pollution_data %>% 
#                   group_by(State, Date.Local) %>% 
#                   summarise("NO2" = mean(NO2.Mean), "O3" = mean(O3.Mean), 
#                             "SO2" = mean(SO2.Mean), "CO" = mean(CO.Mean))

# colnames(pollution_means)[which(names(pollution_means) == "Date.Local")] <- "year"
# colnames(pollution_means)[which(names(pollution_means) == "State")] <- "state"

# write.csv(pollution_means, "small_pollution.csv")

pollution_means <- read.csv("data/small_pollution.csv")
pollution_means$X <- NULL

year_bee_data <- bee_data %>% 
                select(StateName, numcol, year) %>% 
                filter(year >= 2000) %>% 
                filter(year < 2017)
colnames(year_bee_data)[which(names(year_bee_data) == "StateName")] <- "state"

# combine the bee and pollution data frames to a complete data frame
complete_data <- inner_join(pollution_means, 
                            year_bee_data, by = c("year", "state"))

shinyServer(function(input, output) {
  get_label <- reactive({
    molecule <- input$pollutant
    
    label <- "Parts per million"
    if (molecule == "NO2" || molecule == "SO2") {
      label <- "Parts per billion"
    }
    
    label
  })
  
  output$pollution_vs_colonies <- renderPlot({
    pollutant_selected <- input$pollutant
    label <- get_label()
    state_selected <- input$select_state
    
    state_data <- complete_data %>% 
                  filter(state == state_selected)
    
    ggplot(state_data, aes(x = state_data[,input$pollutant], y = numcol)) +
      geom_point(shape = 1) + geom_smooth(method=lm, se=FALSE) +
      ggtitle(paste(pollutant_selected, 
                    "Concentration vs. Number of Colonies -",
                    state_selected)) +
      labs(y = "Number of Colonies", 
           x = paste0(pollutant_selected,
                      " Concentration ", " (", label , ")"))
  })
  
  output$all_states <- renderPlot({
    pollutant_selected <- input$pollutant
    label <- get_label()
    state_selected <- input$select_state
    
    complete_data$numcol <- complete_data$numcol / 1000
    
    ggplot(complete_data, aes(x = complete_data[,input$pollutant],
                              y = numcol)) +
      geom_point(shape = 1) + geom_smooth(method=lm, se=FALSE) +
      ggtitle(paste(pollutant_selected, 
                    "Concentration vs. Number of Colonies - All States")) +
      labs(y = "Number of Colonies (thousand)", 
           x = paste0(pollutant_selected, 
                      " Concentration ", " (", label , ")"))
  })
  
})
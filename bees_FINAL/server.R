# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(leaflet)
library(readxl)
library(lubridate)
library(gridExtra)
library(stringr)

pollution_means <- read.csv("data/small_pollution.csv")
pollution_means$X <- NULL

bee_data <- read.csv("data/vHoneyNeonic_v03.csv")

year_bee_data <- bee_data %>% 
  select(StateName, numcol, year) %>% 
  filter(year >= 2000) %>% 
  filter(year < 2017)
colnames(year_bee_data)[which(names(year_bee_data) == "StateName")] <- "state"

# combine the bee and pollution data frames to a complete data frame
complete_data <- inner_join(pollution_means, 
                            year_bee_data, by = c("year", "state"))

neonic <- read.csv("data/vHoneyNeonic_v03.csv", header = TRUE, sep = ",")
source('data/process.R')

data_phoebe <- read.csv("data/vHoneyNeonic_v03.csv", stringsAsFactors = FALSE)
uni_names <- as.list(unique(data_phoebe$StateName))

bee_data <- read.csv("data/vHoneyNeonic_v03.csv")

df2 <- read.csv('data/Bee_Colony_Loss.csv', stringsAsFactors = FALSE)
df2$X.Total.Annual.Loss <- df2$X.Total.Annual.Loss * 100

df3 <- read.csv('data/vHoneyNeonic_v03.csv', stringsAsFactors = FALSE)
colonies <- df3 %>% select(numcol, year, StateName) %>% rename(State = StateName) %>% mutate(numcol = numcol / 1000)

temp <- read.csv('data/Temperature_small.csv', stringsAsFactors = FALSE)
fixed_georgia <- temp %>% 
  filter(State == "Georgia (State)") %>% 
  mutate(State = "Georgia")
temp <- rbind(temp, fixed_georgia) %>% filter(State != "Georgia (State)")
temp$dt <- as.Date(temp$dt)
state_name <- unique(df3$StateName)

server <- function(input, output) {
  
  dfreact <- reactive({
    df2
  })
  
  output$note <- renderText({
    text <- paste0("<h3>Bee Colonies Lost Percentage by State Between ","<font color=\"#FF0000\"><b>", input$year)
  })
  
  
  
  output$bees <- renderLeaflet({
    df_bees <- dfreact() %>% filter(Year == input$year)
      states <- geojsonio::geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")
      df_bees <- dfreact() %>% filter(Year == input$year)
      df_bees <- merge(states, df_bees, by.x="name", by.y="X.State", all.x=TRUE)
      pr <- filter(df_bees, name == "Puerto Rico")
      df_bees <- filter(df_bees, name != "Puerto Rico")
      df_bees <- rbind(df_bees, pr)
      head(states)
      m <- leaflet(states) %>%
        setView(-96, 37.8, 4) %>%
        addTiles()
      bins <- c(0, 20, 40, 60, 80, 100)
      pal <- colorBin("YlOrRd", domain = df_bees$X.Total.Annual.Loss, bins = bins)
      print(head(df_bees))
      labels <- sprintf(
        "<strong>%s</strong><br/>%g percent of the bees colonies were lost <sup></sup>",
       df_bees$name, df_bees$X.Total.Annual.Loss
      ) %>% lapply(htmltools::HTML)
        
      m <- m %>% addPolygons(
        fillColor = pal(df_bees$X.Total.Annual.Loss),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))
      m %>% addLegend(pal = pal, values = ~df_bees$X.Total.Annual.Loss, opacity = 0.7, title = NULL,
                      position = "bottomright") #%>% add("USA")
  })
  # 
  output$beesbar <- renderPlot({
    df_bar <- dfreact() %>% filter(Year == input$year)
    ggplot(df_bar) + geom_bar(aes(X.State, X.Total.Annual.Loss, fill= X.Total.Annual.Loss), stat = "identity") + coord_flip() +
      labs(x= "USA States", y = "Percentage Lost") + guides(fill=FALSE) + 
      theme(axis.text=element_text(size=12), title = element_text(size = 20), axis.title=element_text(size=14,face="bold")) +
      scale_y_continuous(breaks=seq(0, 100, 10), limits=c(0, 100)) + scale_fill_gradient2(low="#FAD800", high = "#FA7500", limits=c(0,100))
  })
  
  output$temp <- renderPlot({
    if(is.null(input$myPicker)) {
      return(ggplot())
    }
    
    data <- temp %>% filter(dt >= "1990-01-01")
    calculated <- data.frame(numcol=as.numeric(),
                             year=as.numeric(),
                             State.x = character(),
                             AvgTemperature=numeric(),
                             State.y=character()
                             # changeTemp=numeric()
                           )
    calculated$State.x <- as.character(calculated$State.x)
    calculated$State.y <- as.character(calculated$State.y)
    # Input the state that want to be plotted
    
    state_name <- unique(data$State)
    for (val in state_name) {
      filtered <- data %>% 
        filter(State == val) %>% 
        group_by(year = floor_date(dt, "year")) %>% 
        summarize(AvgTemperature=mean(AverageTemperature)) %>% 
        mutate(State = val)
      filtered$year <- as.numeric(format(filtered$year,'%Y'))
      filtered <- filtered %>% 
        arrange(.,year)
        # mutate(changeTemp = AvgTemperature - .$AvgTemperature[1])
      # calculated <- bind_rows(calculated, filtered)
      colonies_filtered <- colonies %>% filter(State == val)
      joined <- inner_join(colonies_filtered, filtered, by = "year", copy = FALSE)
      calculated <- rbind(calculated, joined)
    }
    state <- calculated %>% filter(State.y %in% input$myPicker)

    ggplot(state, aes(x=AvgTemperature, y=numcol)) +
      geom_point() + 
      geom_smooth(method = lm, se=FALSE) + facet_wrap(. ~State.y, scales = "free", shrink = TRUE,
                                                                     as.table = TRUE) +
      labs(x= "Average Temperature (Celsius)", y = "Number of Bees Colonies (thousands)", title = "Correlation Between Average Temperature and Number of Bees Colonies") +
      theme(axis.text=element_text(size=12), title = element_text(size = 20), axis.title=element_text(size=14,face="bold")) +
      scale_y_continuous()
    
    
  })
 
  output$threshold <- renderPlot({
    if(is.null(input$myPicker)) {
      return(ggplot())
    }
    data <- temp
    calculated <- data.frame(year=as.character(),
                             State = character(),
                             maxTemp=numeric(),
                             minTemp=numeric(),
                             Country=character()
                             # changeTemp=numeric()
    )
    # Input the state that want to be plotted
    
    #### Calculate change in avg temp
    state_name <- unique(data$State)
    for (val in state_name) {
      filtered <- data %>% 
        filter(State == val)
      
      filtered$dt <- as.character(filtered$dt)
      
      range <- c(1980:2013)
      for (time in range) {
        get_year<- filtered %>% filter(str_detect(dt, as.character(time)))
        max_temp <- max(get_year$AverageTemperature)
        min_temp <- min(get_year$AverageTemperature)
        get_temp <- data.frame("year"=time, ## time
                               "State" = val, ### val
                               "maxTemp"=max_temp,
                               "minTemp"=min_temp,
                               "Country"="USA"
        )
        calculated <- rbind(calculated, get_temp)
      }
    }
    get_state <- calculated %>% filter(State %in% input$myPicker)
    ggplot(get_state, aes(year), show.legend = FALSE) + geom_point(aes(y=maxTemp)) + geom_point(aes(y=minTemp)) + geom_line(aes(y=maxTemp, color='#FFDD50')) + geom_line(aes(y=minTemp,  color='#FFDD50')) +
      geom_hline(yintercept=34, linetype="dashed", color = "red") + geom_hline(yintercept=13, linetype="dashed", color = "red") +
      facet_wrap(. ~State, scales = "free", shrink = TRUE, as.table = TRUE) +
      labs(x= "Year", y = "Temperature (Celsius)", title = "Max/Min Temperature Overtime by State") +
      theme(axis.text=element_text(size=12), title = element_text(size = 20), axis.title=element_text(size=14,face="bold")) +
      scale_x_continuous(breaks = seq(1975,2015,5)) + guides(color=FALSE)
  
  })
  
  output$phoebe_graph <- renderPlot({
    if (input$radio == "Region") {
      colonies_per_year <- data_phoebe %>% 
        group_by(year, Region) %>% 
        summarise(col_total = sum(numcol, na.rm = TRUE)) 
      ggplot(colonies_per_year, aes(x = year, y = col_total, group = Region, color = Region)) +
        geom_point(size = 1) + geom_line() +
        labs(title = "Total Number of Honeybee Colonies by Region",
             x = "Year",
             y = "Number of Colonies",
             color = "Region") +
        scale_x_continuous(breaks = seq(1991, 2017, by = 2)) +
        theme(axis.text=element_text(size=12), title = element_text(size = 20), 
              axis.title=element_text(size=14,face="bold"))
    } else {
      state_name <- input$select
      colonies_per_year <- data_phoebe %>% 
        filter(StateName == state_name) %>% 
        select(numcol, year)
      ggplot(colonies_per_year, aes(x = year, y = numcol)) +
        geom_point(size = 2, col = "#f4be41") + geom_line(size = 1, col = "#f4c741") + 
        labs(title = paste0("Total Number of Honeybee Colonies in ", state_name),
             x = "Year",
             y = "Number of Colonies") +
        scale_x_continuous(breaks = seq(1991, 2017, by = 2)) + 
        theme(axis.text=element_text(size=12), title = element_text(size = 20), 
              axis.title=element_text(size=14,face="bold"))
    }
  })
  
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
  
  ### MICHELLEEEEE
  output$plot_xy <- renderPlot({
    data <- by_year(neonic)
    x <- "Year"
    y <- "Honey Yield"
    if (!is.null(input$x_xy) && !is.na(input$x_xy)) {
      x <- input$x_xy
    }
    if (!is.null(input$y_xy) && !is.na(input$y_xy)) {
      y <- input$y_xy
    }
    if (!is.null(input$state_xy) && !is.na(input$state_xy) && input$state_xy != "All States") {
      data <- filter_by_state(input$state_xy, neonic)
    }
    
    if(!is.null(input$x_xy) &&
       !is.na(input$x_xy) &&
       !is.null(input$y_xy) &&
       !is.na(input$y_xy) &&
       (input$x_xy == 'Year' || input$y_xy == 'Year')
    ) {
      data <- by_year(data)
    }
    return(plot_var(data, name_to_col(data, x), name_to_col(data, y), paste0(y, " by ", x, " for ", input$state_xy), x, y))
  })
  
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
                      " Concentration ", " (", label , ")")) +
      theme(axis.text=element_text(size=12), title = element_text(size = 20), 
            axis.title=element_text(size=14,face="bold"))
  })
  
  ## KYLE
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
      theme(axis.text=element_text(size=12), title = element_text(size = 20), 
            axis.title=element_text(size=14,face="bold")) +
      labs(y = "Number of Colonies (thousand)", 
           x = paste0(pollutant_selected, 
                      " Concentration ", " (", label , ")")) +
      scale_y_continuous(limits = c(0,100))
  })
  
}



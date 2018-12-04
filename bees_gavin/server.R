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
    text <- paste0("<h3>Bees colonies Lost Percentage by State between ","<font color=\"#FF0000\"><b>", input$year)
  })
  
  output$leaf <- renderLeaflet({
    #if (input$xaxis == "State") {
      states <- geojsonio::geojson_read(x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")
      df_state <- df %>% 
        filter(Country == "USA") %>% 
        count(State)
      m <- leaflet(states) %>%
        setView(-96, 37.8, 4) %>%
        addTiles()
      bins <- c(0, 10, 20, 50, 100, 200, 500, Inf)
      pal <- colorBin("YlOrRd", domain = df_state$n, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%g sightings <sup></sup>",
        df_state$State, df_state$n
      ) %>% lapply(htmltools::HTML)
      
      m <- m %>% addPolygons(
        fillColor = pal(df_state$n),
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
      m %>% addLegend(pal = pal, values = ~n, opacity = 0.7, title = NULL,
                      position = "bottomright") #%>% add("USA")
    #}
     
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
      scale_y_continuous(breaks=seq(0, 100, 10)) + scale_fill_gradient2(low="#FAD800", high = "#FA7500")
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
    ggplot(get_state, aes(year)) + geom_point(aes(y=maxTemp)) + geom_point(aes(y=minTemp)) + geom_line(aes(y=maxTemp)) + geom_line(aes(y=minTemp)) +
      geom_hline(yintercept=34, linetype="dashed", color = "red") + geom_hline(yintercept=13, linetype="dashed", color = "red") +
      facet_wrap(. ~State, scales = "free", shrink = TRUE, as.table = TRUE) +
      labs(x= "Year", y = "Temperature (Celsius)", title = "Max/Min Temperature Overtime by State") +
      theme(axis.text=element_text(size=12), title = element_text(size = 20), axis.title=element_text(size=14,face="bold")) +
      scale_x_continuous(breaks = seq(1975,2015,5))
  
  })
}
# Calculat max / min temp to determine threshold bad for bees.
# Georgia (state)
## A Leaflet map showing total bee colonies loss between periods.


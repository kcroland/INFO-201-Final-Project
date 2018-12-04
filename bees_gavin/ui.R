#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shinyWidgets)
library(shiny)
# state_name <- unique(data$State)
source("server.R")
ui <- fluidPage(
  titlePanel("Honey Bees"),
  sidebarLayout(
    sidebarPanel(
      # Use conditional to change widgets between the tab. Setting conditional value == n to 
      # change accoriding to the tab value below
      conditionalPanel(condition="input.tabselected==2",
                       pickerInput(
                         inputId = "myPicker", 
                         label = "Select/deselect all + format selected", 
                         choices = state_name, 
                         options = list(
                           `actions-box` = TRUE, 
                           size = 10,
                           `selected-text-format` = "count > 3"
                         ), 
                         multiple = TRUE,
                         selected = "Alabama"
                       ),
                       radioButtons("type",
                                     h4("Select chart type:"),
                                     c("Scatter plot", "Time graph"),
                                     selected = "Scatter plot"
                       )
                       
      ),
      conditionalPanel(condition="input.tabselected==3",
                       selectInput(
                         "year",
                         h4("Select the year:"),
                         c("2016/17", "2015/16", "2014/15", "2013/14", "2012/13", "2011/12", "2010/11"),
                         selected = "2016/17"
                        ),
                       radioButtons("chart",
                                    h4("Select chart type:"),
                                    c("Map", "Bar"),
                                    selected = "Map"
                                    )
                       
      )
      
    ),
    mainPanel(
      # Set the tab to change between two visuals
      tabsetPanel(
        # The "value" notify widget conditional panel to return/show what widget
        tabPanel("Bees", value=3, htmlOutput("note"), conditionalPanel(condition = "input.chart=='Map'", leafletOutput("bees", height="700px")), 
                 conditionalPanel(condition="input.chart=='Bar'", plotOutput("beesbar", height="700px"))),
        tabPanel("Scatter Plot", value=2, conditionalPanel(condition="input.type=='Scatter plot'", plotOutput("temp", height="700px")), 
                 conditionalPanel(condition="input.type=='Time graph'", plotOutput("threshold", height="700px"))),
        id = "tabselected"
        )

    )
  )
)


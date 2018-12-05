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
library(shinythemes)
# state_name <- unique(data$State)
source("server.R")
ui <- fluidPage(
  navbarPage(
    title = "SAVE BEES!!",
    theme = shinytheme("united"),
    tabPanel("Overview",
             mainPanel(
               tags$h1("Honeybee Populations are Declining!"),
               tags$div(
                 tags$h3("Project Description"),
                 tags$b("Group BE4: Kyle Roland, Gavin Sreesangkom, Michelle Lee, & Phoebe Ng"),
                 tags$br(),
                 tags$b("Project Purpose:"),
                 tags$p("The purpose of this project is to spread awareness of the declining honeybee population.
                  There are many reasons why the honeybees are dying, but in this project we analyze three specific variables including pollution levels, use of neonic pesticides, 
                  and change in air temperature, and how these variables correlate with the total number of honeybee colonies and their ability to pollinate. 
                  Why are the bees important? A world without bees would be a world without your favorite fruit or flower. Honeybees are essential to our ecosystem, 
                  responsible for pollinating about 80% of all flowering plants. The other pollinators are other wild insects or birds. As the main pollinators in our ecosystem, 
                  it is crucial that we maintain the health of these creatures. Thus, our team will analyze factors that have been speculated to contribute to the bees’ decline and ultimately save the bees."), 
                 tags$b("Data Sources: Honeybees and Neonic Pesticides"),
                 tags$p("This dataset is taken from Kaggle.com and includes information about the number of honey-producing colonies, 
                        the total production of honey, the amounts of specific types of neonic pesticides, the state, and the year. 
                        This dataset aggregates data collected by The National Agricultural Statistics Service (NASS), which is the primary data reporting group for the USDA. 
                        This dataset also takes information from the USGS through its pesticide national synthesis project which estimates the annual agricultural pesticide use."),
                 tags$b("Annual Bee Colony Lost Data"),
                 tags$p("This dataset is taken from data.world under the project “Honey Bees and Apiculture”. 
                        It includes information about the total annual loss of bees and the number of colonies specific to each state and year. 
                        The original data was retrieved from the United States Department of Agriculture."),
                 tags$b("U.S. Pollution Data"),
                 tags$p("This dataset was accessed from Kaggle.com. It includes information about the amounts of four major pollutants (nitrogen dioxide, sulphur dioxide,
                        carbon monoxide and ozone) in different cities, states and years. The original data for this was taken from the U.S. Environmental Protection Agency (EPA)."),
                 tags$b("Climate Change: Earth Surface Temperature Data"),
                 tags$p("This dataset was found on Kaggle.com and includes information on the average temperature, city, country, and longitude and latitude coordinates. 
                        This data originates from NOAA, NASA, and GISTEMP, but later compiled by Berkeley Earth, affiliated with the Lawrence Berkeley National Laboratory."),
                 tags$h3("Questions explored:"),
                 tags$ul(
                   tags$li("What states have the greatest average decline in bee population?"),
                   tags$li("Is there a trend in which areas are declining the most in bee population, such as geographical location or state?"),
                   tags$li("How do different pesticides correlate with honey production?"),
                   tags$li("Is there a relationship between pollution levels and bee population?")
                 )
                 )
               )),
    tabPanel("Colonies",
             sidebarLayout(
               sidebarPanel(
                 
                 radioButtons("radio", label = h3("View by"),
                              choices = list("Region", "State")),
                 conditionalPanel(
                   condition = "input.radio == 'State'",
                   selectInput("select", label = h3("Select a State"), 
                               choices = uni_names, 
                               selected = 1)
                 )
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 tabsetPanel(
                   tabPanel("Graph", plotOutput("phoebe_graph", height = "700px")),
                   tabPanel("Summary",
                            tags$br(),
                            tags$p("This visualization compares the number of honeybee colonies over a span of 27 years (1991-2017). 
                                   In this chart, time is plotted by year along the x-axis and the number of honeybee colonies is plotted along the y-axis. "),
                            tags$br(),
                            tags$p("There are two radio buttons available on the left side that provide the option to view the data by region or by state. 
                                   If a state is selected, a drop down menu will appear for states available in the dataset. 
                                   Filtering by state can allow students to analyze common trends over the last 17 years across the United States. "),
                            tags$br(),
                            tags$p("From our research, California, Iowa, Nebraska, Minnesota, and Missouri are the top five states that grow the most produce. 
                                   Interestingly, the chart shows that these states all have a downward trend in bee population.")
                            )
               )
             )
          )
    ),
    tabPanel("Map",
             sidebarLayout(
               sidebarPanel(
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
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 tabsetPanel(
                   tabPanel("Graph", htmlOutput("note"),conditionalPanel(condition = "input.chart=='Map'", leafletOutput("bees", height="700px")), 
                            conditionalPanel(condition="input.chart=='Bar'", plotOutput("beesbar", height="700px"))),
                   tabPanel("Summary",
                            tags$br(),
                            tags$p("This section analyzes the number of bee colonies lost by percentage dependent on the state. 
                                    On the left side, there is a drop down menu for the user to 
                                    select which two years to calculate how many bees were lost from one year to the next. 
                                    There is also a radio button that changes the chart type.
                                   "),
                            tags$br(),
                            tags$p("If the map chart type is selected, a map is displayed and color codes the areas with relevant data. 
                                   The darker the color, the greater the percentage lost in bee colony from one year to the next. 
                                   By hovering over a specific state, the state name and specific number of the percent of bee colonies that were lost is listed."),
                            tags$br(),
                            tags$p("If the bar chart type is selected, the percentage lost is listed on the x-axis and the state is listed on the y-axis. 
                                   This is also color coded to reflect greater losses in the bee colony populations as the color deepens.")
                            )
                 )
               )
             )
    ),
    tabPanel("Pesticides",
             sidebarLayout(   # layout the page in two columns
               sidebarPanel(  # specify content for the "sidebar" column
                 uiOutput('state_selector_xy'),
                 uiOutput('x_xy_selector'),
                 uiOutput('y_xy_selector')
               ),
               mainPanel( 
                 tabsetPanel(
                 tabPanel("Graph", plotOutput("plot_xy", height = "700px")),
                 tabPanel("Summary",
                          tags$br(),
                          tags$p("In this scatterplot, we are examining how honey production is correlated with the amount of neonic pesticides. 
                                 Bees produce honey to store as food for cold winter periods. Since honey is made when bees convert and store the
                                 nectar it gathers from flowers into the honeycombs, thus, honey is an indicator of the bees’ ability to pollinate. 
                                 Ultimately, this scatterplot examines how neonic pesticides correlate with bees’ ability to pollinate."),
                          tags$br(),
                          tags$p("The user can interact with the drop-down panels available on the left. These panels will filter by 
                                 state as well as select what will be graphed on the x and y-axis. The options available for the x or
                                 y-axis include the year, the amount of neonic pesticides (in kg), and the amount of honey production 
                                 (honey yield in pounds)."),
                          tags$br(),
                          tags$p("Looking at overall states, as the years progress, the amount of honey yield has a downward trend. 
                                 However, when also looking at the progression in years, the amount of neonic pesticides increases 
                                 exponentially until around 2015 and then steeply declines. "),
                          tags$br(),
                          tags$p("When comparing the honey production to the amount of neonic pesticides, there is a general trend that as honey 
                                 yield increases above 2375 pounds, the smaller the amount of neonic pesticides. However, for honey yield that is 
                                 below 2375 pounds, there is a very expansive range of how much honey is produced and how much neonic pesticides are used."))
               )
               )
             )
    ),
    tabPanel("Temperature",
             sidebarLayout(
               sidebarPanel(
                 pickerInput(
                   inputId = "myPicker", 
                   label = "Select States: Select/deselect all + format selected", 
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
               mainPanel( 
                 tabsetPanel(
                   tabPanel("Graph", conditionalPanel(condition="input.type=='Scatter plot'", plotOutput("temp", height="700px")), 
                            conditionalPanel(condition="input.type=='Time graph'", plotOutput("threshold", height="700px"))),
                   tabPanel("Summary",
                            tags$br(),
                            tags$p("The population of bees is affected by various factors. And temperature is one of them. Too high? 
                                   Bees' ability to learn is hindered. Too low? Bees develop abnormal flying habits.
                                  These charts analyze the correlation between
                                   average temperature and the number of bee colonies. On the left side, users can interact with a drop-down
                                   panel to select as many states as the user would like to compare. There are also radio buttons that allow
                                   the user to toggle between scatter plots and line graphs plotting time."),
                            tags$br(),
                            tags$p("For the scatter plot graphs, the average temperature (in Celsius) is on the x-axis, and the number of bee
                                   colonies (in thousands) are on the y-axis. Interestingly, Maryland, New Jersey, Montana, North Carolina,
                                   North Dakota, Oklahoma, Oregon, Tennessee, Washington, and Wyoming are the only states that had flat or
                                   increasing number of bee colonies as the temperature also increases. All the other states had smaller 
                                   bee populations as the average temperature increased. An important thing to note is that some states 
                                   did not many many data point to pull from and thus, the scale fluctuates per chart. This can change
                                   how steep the line of best fit appears."),
                            tags$br(),
                            tags$p("For the time graphs, the year is on the x-axis and the temperature in Celsius is on the y-axis.
                                   After selecting a state, the app will plot the minimum and maximum temperature over several years
                                   in two different lines. The chart displays two red lines which indicate the maximum and minimum 
                                   temperatures for most bees can to operate normally, which is around 34 degrees Celsius to 13 degrees 
                                   Celsius. The graph is expected to show what states' changes in maximum and minimum temperature might
                                   pose as a threat to bee colonies."))
                 )
               )
             )
    ),
    tabPanel("Pollution",
             sidebarLayout(   # layout the page in two columns
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
                 tabsetPanel(
                   tabPanel("Graph", plotOutput("pollution_vs_colonies", height = "450px"),
                            plotOutput("all_states", height = "450px")),
                   tabPanel("Summary",
                            tags$br(),
                            ("Why might pollution impact number of honeybee colonies? A study found on "),
                            tags$a(href="https://www.sciencedaily.com/releases/2016/07/160706131924.htm", "Science Daily"),
                            ( "claims that “Bees'ability to forage decreases as air pollution increases”. The study explains that pollution
                                    has an impact on molecules emitted by plants that bees use for locating. This molecular alteration
                                    causes confusion with the bees and leads to overall lower pollinating efficiency. On this page we
                                    compare pollution data with number of colonies to explore this claim."),
                            tags$br(),
                            tags$br(),
                            
                            tags$p("On the left-hand side of the page there are dropdown menus for the type of pollutant to view as well as in which state. 
                                   The data for each state (both in pollution in total colonies) are grouped and then averaged by years. 
                                   For each state there are up to 17 data points, one for each year, of average number of colonies and 
                                   pollutant concentration of that year. The upper plot shows the data for a single state whereas the 
                                   lower plot shows the data for all states. *Lower plot modified slightly to exclude some outliers that
                                   disproportionately stretched the y-axis"),
                            tags$br(),
                            tags$p("It is important to note the limitations that come with the combination of these two data sets. Not all states have 
                                   average colony numbers for each year from 2000 – 2016, and thus these subsets may not provide as reliable correlations 
                                   (if any at all). These visualizations only give a glimpse at the cause behind colony collapse disorder. 
                                   While it may be interesting to explore the relationship between pollution and total colonies, comparing
                                   pollution to a measurement of pollinating efficiency may have proven to provide better conclusions on the impact of atmospheric gases."))
                 )
               )
             )
    )
    
  )

)

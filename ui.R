# load shiny

library("shiny")


my_ui <- fluidPage(
  titlePanel("Comparing Neonic Pesticides, Honey Production, and Year"),  
  sidebarLayout(   # layout the page in two columns
    sidebarPanel(  # specify content for the "sidebar" column
      uiOutput('state_selector_xy'),
      uiOutput('x_xy_selector'),
      uiOutput('y_xy_selector')
    ),
    mainPanel(     # specify content for the "main" column
      plotOutput('plot_xy')
    )
  )
)

shinyUI(my_ui)

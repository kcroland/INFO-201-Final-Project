# load shiny

library("shiny")


my_ui <- fluidPage(
  titlePanel("Title"),  
  sidebarLayout(   # layout the page in two columns
    sidebarPanel(  # specify content for the "sidebar" column
    ),
    mainPanel(     # specify content for the "main" column
    )
  )
)

shinyUI(my_ui)

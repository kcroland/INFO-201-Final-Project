## load shiny
library("shiny")
library("ggplot2")
library("ggmap")
library("maps")
library("mapdata")
library("dplyr")

# Reading vHoneyNeonic
neonic <- read.csv("data/vHoneyNeonic_v03.csv", header = TRUE, sep = ",")

my_server <- function(input, output) {
}


shinyServer(my_server)

# Reading vHoneyNeonic
setwd('C:/Users/miche/Desktop/INFO_201BF/INFO-201-Final-Project');

library('ggplot2')

plot_var <- function(data_mich, x_data, y_data, title, x_label, y_label){
  ggplot(data_mich, aes(x=x_data, y=y_data)) +
    geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) +
    ggtitle(title) +
    labs(y=y_label, x=x_label)
}

# Get for State
filter_by_state <- function(state, data_mich) {
  data_mich[data_mich$StateName==state,]
}

# aggregate values by year
by_year <- function(data_mich) {
  data_mich$nAllNeonic[is.na(data_mich$nAllNeonic)] <- 0
  aggregate(list(yieldpercol=data_mich$yieldpercol, nAllNeonic=data_mich$nAllNeonic), by=list(year=data_mich$year), FUN=sum, na.rm=TRUE, na.action=NULL)
}

name_to_col <- function(data_mich, name) {
  if(name == "Year") {
    return(data_mich$year)
  }
  if(name == "Honey Yield") {
    return(data_mich$yieldpercol)
  }
  if(name == "Amount of Neonic Pesticides") {
    return(data_mich$nAllNeonic)
  }
}


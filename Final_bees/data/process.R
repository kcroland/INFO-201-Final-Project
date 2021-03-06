# Reading vHoneyNeonic

library('ggplot2')

plot_var <- function(data, x_data, y_data, title, x_label, y_label){
  ggplot(data, aes(x=x_data, y=y_data)) +
    geom_point(shape=1) + geom_smooth(method=lm, se=FALSE) +
    ggtitle(title) +
    labs(y=y_label, x=x_label) + 
    theme(axis.text=element_text(size=12), title = element_text(size = 20), 
                                       axis.title=element_text(size=14,face="bold"))
}

# Get for State
filter_by_state <- function(state, data) {
  data[data$StateName==state,]
}

# aggregate values by year
by_year <- function(data) {
  data$nAllNeonic[is.na(data$nAllNeonic)] <- 0
  aggregate(list(yieldpercol=data$yieldpercol, nAllNeonic=data$nAllNeonic), by=list(year=data$year), FUN=sum, na.rm=TRUE, na.action=NULL)
}

name_to_col <- function(data, name) {
  if(name == "Year") {
    return(data$year)
  }
  if(name == "Honey Yield") {
    return(data$yieldpercol)
  }
  if(name == "Amount of Neonic Pesticides") {
    return(data$nAllNeonic)
  }
}


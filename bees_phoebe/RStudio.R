library(dplyr)
library(ggplot2)

data <- read.csv("vHoneyNeonic_v03.csv", stringsAsFactors = FALSE)
uni_names <- as.list(unique(data$StateName))
 


colonies_per_year <- data %>% 
            group_by(year, Region) %>% 
            summarise(col_year_total = sum(numcol, na.rm = TRUE)) 


ggplot(colonies_per_year, aes(x = year, y = col_year_total / 1000, group = Region, color = Region)) +
  geom_point(size = 1) + geom_line() +
  labs(title = "Total Number of Honey Bee Colonies",
       x = "Year",
       y = "Number of Colonies",
       color = "Region") +
  scale_x_continuous(breaks = seq(1991, 2016, by = 4))
  



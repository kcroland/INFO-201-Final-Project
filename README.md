# INFO-201-Final-Project
INFO 201 Final Project - Autumn 2018
[View Project](https://gavinsreeuw.shinyapps.io/BE4_Project/?fbclid=IwAR2_XmNXZihhgALsZhXx_Rx5DzW52_K8abRdNiQodFu7j_ayzW-MkuAnSdY)

Group members: Kyle Roland, Gavin Sreesangkom, Michelle Lee, & Phoebe Ng

## Project Overview

### What's happening to honeybees?
![Honeybee](https://c1.staticflickr.com/1/210/519742656_0b2323bc8e_z.jpg?zz=1)

While exploring final project ideas, our group came to a consensus to research the factors behind [colony collapse disorder](https://www.epa.gov/pollinator-protection/colony-collapse-disorder) (CCD). We all knew about this phenomenon of colony disappearance and its inevitable consequences, however we didn't yet know why it was happening.

Through research we found that there are many factors that play into the decline of honeybee populations. In our project, we chose to explore a few of these factors including pesticides, temperature, and pollution to see if we could make any inferences that would back the claims that we discovered in different studies.

## Data Overview

Several datasets were used for this project to explore the factors behind colony collapse disorder. To view the datasets in their entirety, please view their respective linked sources.

* __Honeybees and Neonic Pesticides__: Includes information about the number of honey-producing colonies, the total production of honey, the amounts of specific types of neonic pesticides, the state, and the year. Also takes information from the USGS through its pesticide national synthesis project which estimates the annual agricultural pesticide use. [Source](https://www.kaggle.com/kevinzmith/honey-with-neonic-pesticide/home)

* __Annual Bee Colony Lost__: Includes information about the total annual loss of bees and the number of colonies specific to each state and year. [Source](https://data.world/finley/honey-bees-and-apiculture)

* __U.S. Pollution__: Includes information about the amounts of four major pollutants (nitrogen dioxide, sulphur dioxide, carbon monoxide and ozone) in different cities, states and years. This data is modified to relevant pollutant concentration means (becomes small_pollution.csv) to shrink the file size. [Source](https://www.kaggle.com/sogun3/uspollution )

* __Climate Change: Earth Surface Temperature__:  Includes information on the average temperature, city, country, and longitude and latitude coordinates.  [Source](https://www.kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data)

## Technical Information

__Format__: This project is displayed through Shiny app. Visualizations (including maps, line, bar, and scatter plots) in this app are interactive and give the users the ability to explore further through changing filters and possibly the type of visualization itself.

__Data__: Read as .csv files then manipulated to relevant information and filtered by user selected input values.

__Libraries__: To view the used libraries, please view the source code - "bees_FINAL/server.R"

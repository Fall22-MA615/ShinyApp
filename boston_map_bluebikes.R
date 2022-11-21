library(ggplot2)

MainStates <- map_data("state")
MassStates <- MainStates[MainStates$region == 'massachusetts', ]

blue <- read.csv('/Users/priamvyas/Desktop/MSSP/615 Data Science in R/Shiny App/bluebikes_data.csv')
blue_docks_reduced <- blue$Total_docks /1000
 
ggplot() + 
  geom_polygon(data=MassStates, aes(x=long, y=lat),
                color="black", fill="lightblue" ) +
  coord_map(xlim = c(-71.25, -70.75),ylim = c(42.25, 42.6)) + 
  geom_point(data=blue, aes(x=Longitude, y=Latitude), color="red")



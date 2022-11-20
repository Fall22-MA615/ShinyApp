library(usmap)
library(ggplot2)

#read in bluebike data 
#https://data.boston.gov/dataset/blue-bike-stations/resource/d0121b02-ac37-4426-a42b-4a6d18d8676d
data <- read.csv("/Users/dz/Documents/MSSP/MA615/Shiny/Blue_Bike_Stations.csv")
blue <- data[,-8] #remove unecessary column

#find empty cells in District column and insert correct one
grep("^[[:space:]]*$",blue$District)
blue[38,7] <- "Malden"
blue[78,7] <- "Salem"

#MA counties
macounty <- countypov %>%
  filter(abbr=="MA") %>%
  select(county)

#add county to dataset
blue$County <- ifelse(blue$District=="Boston" | blue$District=="Chelsea" | blue$District=="Revere", "Suffolk",
       ifelse(blue$District=="Salem", "Essex", ifelse(blue$District=="Brookline", "Norfolk", "Middlesex")))

#add fips to dataset
blue$fips <- ifelse(blue$County=="Suffolk", "25025", ifelse(blue$County=="Essex", "25009",
                    ifelse(blue$County=="Norfolk", "25021","25017")))

#graph
plot_usmap(data = blue, values = "Total_docks", include = "MA", color = "blue") + 
  scale_fill_continuous(low = "white", high = "blue", name = "Frequency of Bluebike Docks", label = scales::comma) + 
  labs(title = "Massachusetts") +
  theme(legend.position = "right")
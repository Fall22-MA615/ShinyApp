library(shiny)
library(dplyr)
library(usmap)
library(ggplot2)

#read in bluebike data 
#https://data.boston.gov/dataset/blue-bike-stations/resource/d0121b02-ac37-4426-a42b-4a6d18d8676d


blue <- read.csv("/Users/jiunlee/MSSP22/MA615/Projects/ShinyApp/Blue_Bike_Stations.csv")
blue <- blue[,-8] #remove unecessary column

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
#shinyApp
ui <- navbarPage("BLUE BIKE STATIONS", collapsible = TRUE, inverse = TRUE, theme = shinytheme("cerulean"),
                 tabPanel("1",
                          fluidPage(
                            titlePanel("Frequency of BlueBike Docks"),   
                            plotOutput("map"))),
                 tabPanel("2",
                          fluidPage(
                            titlePanel("Table"),   
                            tableOutput("table"))),
                 tabPanel("3",
                          fluidPage(
                            titlePanel("?"),   
                            
                           )))

server <- function(input, output, session) {
  output$table <- renderTable(head(blue))
  output$map <- renderPlot({plot_usmap(data = blue, values = "Total_docks", include = "MA", color = "blue") + 
      scale_fill_continuous(low = "white", high = "blue", name = "Frequency of Bluebike Docks", label = scales::comma) + 
      labs(title = "Massachusetts") +
      theme(legend.position = "right")})
}

shinyApp(ui, server)



# 
# tabPanel("3",
#          fluidPage(
#            tabsetPanel(
#              tabPanel("Study Habit"),
#              tabPanel("Space Preference - Mid & Final Terms"),
#              tabPanel("Space Preference - Most Days"),
#              tabPanel("Space Preference - Student Submissions")
#            ))), 



#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above. A new window should appear with the app. 
# For a better viewing exprience, click 'Open in Browser' in the new window.
# This will open the app in a browser window.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
#  This can be run if you have R/Rstudio on your computer.
#  If it does not run properly, check that the necessary R packages are installed.
#  These are listed at the start of the script and loaded with "library()". 
#  They need to be installed on your machine in order to be loaded.
#  To install, run the following code:
#
#  install.packages("shiny")
#  install.packages("tidyverse")
#  install.packages("DT")
#
#  You may also want to ensure that the data file that is being loaded at the start
#  of the script is located where the code indicates.
#


# load packages
library(shiny)
library(tidyverse)
library(RColorBrewer)
library(sf)


# load data for app
ParkingCounts <-  read_csv("//coc/cdd/E&T/_PTDM/bgridley/ParkingStudy/garagecounts_2017_reformat.csv")

# make the hours field numeric
ParkingCounts$hour <- as.numeric(ParkingCounts$hour)

# store the unique hours for the data
hours <- unique(ParkingCounts$hour)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Parking Garage Occupancy on a Typical Day"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Include clarifying text ----
      helpText("Note: the counts for each garage were not taken on the same day."),
      
      
      # Input: Animation with custom interval (in ms) ----
      # to control speed, plus looping
      sliderInput("hour", "Time (24-hour clock):",
                  min = min(hours),
                  max = max(hours),
                  value = min(hours), 
                  sep = "",
                  step = 100,
                  animate =
                    animationOptions(interval = 800, loop = TRUE))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Bar plot of the individual garages ----
      plotOutput("barPlot"),
      
      # Output: Single bar plot of the total occupancy count ----
      plotOutput("overall"),
      
      # Output: map plot ----
      plotOutput("map")
      
    )
  )
)


server <- function(input, output) {
  
  # all garages bar plot 
  output$barPlot <-  renderPlot({
    
    data <- filter(ParkingCounts, hour == input$hour)
    
    ggplot(data, aes(x = ParkingFacility, y = occupancy)) +
      geom_bar(stat = "identity", fill = "firebrick1") +
      scale_y_continuous(limits = c(0,2500), labels = scales::comma) + 
      theme(panel.background = element_blank(),
            text = element_text(size = 18),
            axis.line.y =  element_line(size = .5),
            axis.line.x = element_blank(),
            panel.grid = element_blank(),
            axis.ticks.x = element_blank(),
            panel.border = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 28)) + 
      labs(title = paste(c("Hour:"), input$hour), 
           x = "Individual Parking Garages", y ="")
    
  })
  
  # total count bar plot
  output$overall <- renderPlot({
    
    # create a summary by hour file to be used for this
    summarydata <- ParkingCounts %>%
      group_by(hour) %>%
      summarise(`Citywide Parking Occupancy` = sum(occupancy), `x` = c("X"))
    
    summarydataplot <- filter(summarydata, hour == input$hour)
    
    ggplot(summarydataplot, aes(x = x, y= `Citywide Parking Occupancy`)) +
      geom_bar(stat = "identity", fill = "dodgerblue1", width = .2,
               position = position_nudge(x = -0.35)) + 
      coord_flip() +
      scale_y_continuous(limits = c(0,20000), labels = scales::comma) +
      geom_text(aes(label = scales::comma(summarydataplot$`Citywide Parking Occupancy`)), 
                fontface = c("bold"), size = 7, color = c("black"),
                position = position_nudge(x = -.2)) +
      theme(panel.background = element_blank(),
            text = element_text(size = 18),
            axis.line.x =  element_line(size = .5),
            axis.line.y = element_blank(),
            panel.grid = element_blank(),
            axis.ticks.y = element_blank(),
            panel.border = element_blank(),
            axis.text.y = element_blank()) +
      labs(x = "", y = "All Garages")
    
  })
  
  # map of occupancy
  output$map <- renderPlot({
  
    #load the basemap file of Cambridge neighborhoods
    cam_nhoods <- sf::read_sf("//coc/cdd/E&T/_PTDM/bgridley/ParkingStudy/BOUNDARY_CDDNeighborhoods.geojson")
    
    # make the N_HOOD field numeric for the join
    cam_nhoods$N_HOOD <- as.numeric(cam_nhoods$N_HOOD)
    
    # create a summary by neighborhood file to be used for this
    summaryneighborhood <- ParkingCounts %>%
      group_by(hour, N_HOOD) %>%
      summarise(NHOODoccupancy = sum(occupancy, na.rm = TRUE))
    
    maxocc_byNeighb <- summaryneighborhood %>%
      group_by(N_HOOD) %>%
      summarise(MaxOCC = max(NHOODoccupancy))
    
    summaryneighborhood2 <- left_join(summaryneighborhood, maxocc_byNeighb,  by = c("N_HOOD"))
    
    summaryneighborhood3 <- summaryneighborhood2 %>%
      mutate(Occ_perc = round((NHOODoccupancy/MaxOCC)*100,0))
    
    summaryneighborhoodplot <- filter(summaryneighborhood3, hour == input$hour)
    
    # now need to join to bring data into mapping file
    cam_nhoods_joined <- left_join(x = cam_nhoods, y = summaryneighborhoodplot, 
                                   by = c("N_HOOD"))
    
    ggplot(cam_nhoods_joined) +
      geom_sf(aes(fill = Occ_perc), color = "black", size = 0.01) +
      scale_fill_distiller("Percent Occupied", limits = c(0,100), 
                           palette = "Blues", direction = 1,
                           na.value = "white") +
      labs(title = paste(c("Hour:"), input$hour),
           x = "Occupancy by Neighborhood") +
      theme(text = element_text(size = 18),
            panel.background = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.border = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.7, size = 22))
    
  })
  
}

shinyApp(ui, server)

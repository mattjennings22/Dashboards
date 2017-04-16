#BUAN 5210
#Final Project Shiny App
#Matt Jennings
#December 10 2016

setwd('C:/Users/Matt/Documents/GRAD SCHOOL/BUAN 5210/Final Project')
rm(list=ls(all=TRUE))
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shinyjs)
library(lazyeval)
library(shiny)
library(scales)
library(reshape2)

#Setup and data manipulation
##Load data 
trip <- read.csv("trip.csv")
weather <- read.csv("weather.csv")
station <- read.csv("station.csv")
end_station <- read.csv("end_station.csv")

##Merging the data into one place
##Guidance provided by: 
##https://www.kaggle.com/parryfg/d/benhamner/sf-bay-area-bike-share/merging-weather-data-into-trip-data-set

#Get "date" variable in weather and trip datasets into same format
weather$date <- mdy(weather$date)
trip$start_date <- mdy_hm(trip$start_date)
trip$end_date <- mdy_hm(trip$end_date)

trip$date <- trip$start_date
trip$date <- as.Date(trip$date)

#Get zip code variable in trip df into same format as other datasets
trip$zip_code <- as.numeric(levels(trip$zip_code))[trip$zip_code]
#NAs introduced and some prior coding errors of zip_code maintained 

#Get the "city" variable into the trip dataset using the common variable "id" in the station dataset
#Also adding starting and ending station elevations

trip$date <- as.Date(trip$start_date)
trip$id2 <- trip$id
trip$id <- trip$start_station_id 
trip$id3 <- trip$end_station_id
trip <- left_join(trip, station, by = c ("id"))
trip <- left_join(trip, end_station, by = c ("id3"))

#Now get the "city" variable into the weather dataset using zip_code
zip_code <- unique(weather$zip_code)
city <- c ("San Francisco", "Redwood City", "Palo Alto", "Mountain View", "San Jose")
index <- cbind(city, zip_code)   
weather <- merge(weather, index, by = "zip_code")

#Now merge weather and trip data based on "city" and "date" variables
trip <- left_join(trip,weather, by = c("date", "city"))

dataset <- trip

#Graphs

#Plot 3
station_trips <- dataset %>%
  select(start_station_name, city, dock_count) %>%
  group_by(start_station_name, city) %>%
  summarize(total_trips=n(), dock_count=mean(dock_count))

trips_per_dock <- station_trips %>%
  select(city, total_trips, dock_count) %>%
  group_by(start_station_name, city) %>%
  summarize(trips_per_dock=sum(total_trips)/sum(dock_count))

#Plot 2
dataset$day <- weekdays(as.Date(dataset$date))

day <- dataset %>%
  select(day, duration) %>%
  group_by(day) %>%
  summarize(total_trips=n(), avg_duration=mean(duration)/60)

#Plot 1
dataset$month <- format(as.Date(dataset$date), "%Y-%m")

monthly_subscription <- dataset %>%
  select(month, subscription_type) %>%
  group_by(month, subscription_type) %>%
  summarize(total_trips=n())

#Plot 4
dataset$longride <- ifelse(dataset$duration>1800,1,0)

long_rides <- dataset %>%
  select(longride) %>%
  group_by(longride) %>%
  summarize(total_trips=n())

city_long_rides <- dataset %>%
  select(city, longride) %>%
  group_by(city, longride) %>%
  summarize(total_trips=n())

city_long_percent <- city_long_rides %>%
  select(city, longride, total_trips) %>%
  group_by(city) %>%
  summarize(longrides=total_trips[longride==1], total_trips=sum(total_trips))

city_long_percent <- city_long_percent %>%
  select(city, longrides, total_trips) %>%
  group_by(city) %>%
  summarize(longride_percent=longrides/total_trips)

city_subscription <- dataset %>%
  select(city, subscription_type) %>%
  group_by(city, subscription_type) %>%
  summarize(total_trips=n())

city_subscription_percent <- city_subscription %>%
  select(city, subscription_type, total_trips) %>%
  group_by(city) %>%
  summarize(subscription_types=total_trips[subscription_type=="Customer"], total_trips=sum(total_trips))

city_subscription_percent <- city_subscription_percent %>%
  select(city, subscription_types, total_trips) %>%
  group_by(city) %>%
  summarize(customer_percent=subscription_types/total_trips)

city_melt <- left_join(city_subscription_percent,city_long_percent, by = c("city"))

city_melt <- melt(city_melt)

#UI
body <- dashboardBody(
  fluidRow(
    column(width = 6,
           box(
             title = "Trips dip in the winter, no growth for customers",
             width = NULL, status = "success", solidHeader = TRUE,
             plotOutput("subscription_plot")
           ),
           box(
             width = NULL, status = "success",
             radioButtons("subscriptionInput", "Subscription Type",
                          choices = c("Subscriber", "Customer"),
                          selected = "Subscriber", inline = TRUE)
           )
    ),

    column(width = 6,
           box(
             title = "Weekdays have more riders, shorter rides",
             width = NULL, status = "warning", solidHeader = TRUE,
             plotOutput("weekday_plot")
           ),
           box(
             width = NULL, status = "warning",
             radioButtons("variableInput", "Variable",
                          choices = c("Total Trips", "Average Trip Length"),
                          selected = "Total Trips", inline = TRUE)
           )
    )

  ),
  fluidRow(
    column(width = 6,
           box(
             title = "Busy stations need more docks", width = NULL, 
             status = "primary", solidHeader = TRUE,
             plotOutput("dock_plot")
           )
           
    ),
    column(width = 6,
           box(
             title = "Palo Alto has more customers, more long rides", width = NULL, status = "success",
             solidHeader = TRUE,
             plotOutput("percentage_plot")
           )

    )

    
  )
)



ui <- dashboardPage(
  dashboardHeader(title = "Bike Share Dashboard: Analysis to Improve Ridership", titleWidth = 700),
  dashboardSidebar(disable = TRUE),
  body
)

#Server
server <- function(input, output, session) {
  output$dock_plot <- renderPlot({
    
    
    ggplot(trips_per_dock, aes(x = trips_per_dock, y = reorder(city, trips_per_dock))) +
      geom_point(aes(color = city), size=2) + 
      guides(color = FALSE) +
      xlab("Trips taken per dock") +
      ylab("Station city") +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))
  })
  
  output$subscription_plot <- renderPlot({
    monthly_plot <- monthly_subscription %>%
      filter(subscription_type == input$subscriptionInput)

    ggplot(data=monthly_plot, aes(x=month, y=total_trips, color = subscription_type)) +
      geom_point(aes(color = subscription_type),size=2) +
      geom_line(aes(color = subscription_type, group = subscription_type)) +
      guides(color=FALSE) +
      scale_x_discrete(breaks=c("2013-08","2014-02","2014-08","2015-02","2015-08")) +
      xlab("Month") +
      ylab("Total Trips") 

  })
  output$percentage_plot <- renderPlot({
    ggplot(city_melt, aes(x=city,y=value, fill=variable))+
      geom_bar(stat="identity",position="dodge") +
      xlab("City") +
      ylab("Percentage") +
      theme(axis.text.x=element_text(angle=90)) +
      scale_fill_discrete(name="Variable",
                          labels=c("Percentage\nof rides\nby customers\n", 
                                   "Percentage\nof rides\nover 30 mins\n"))
  })

  plotType <- reactive({
    switch(input$variableInput,
           "Total Trips" =
             ggplot(day, aes(x=reorder(day, total_trips), y=total_trips)) +
             geom_bar(aes(fill = day), stat = "identity") +
             coord_flip() +
             guides(fill = FALSE) +
             ylab("Total Trips") +
             xlab("Day of the Week"),
            "Average Trip Length" =    
             ggplot(day, aes(x=reorder(day, total_trips), y=avg_duration)) +
             coord_flip() +
             geom_bar(aes(fill = day), stat = "identity") +
             guides(fill = FALSE) +
             ylab("Average duration (minutes)") +
             xlab("Day of the Week")) 
  })
  output$weekday_plot <- renderPlot({
    plotType()

  })

} 

#App
shinyApp(ui = ui, server = server)
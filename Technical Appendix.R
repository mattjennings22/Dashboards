#Matt Jennings
#BUAN 5210
#Final Project
#12/2/16

#Setup
setwd('C:/Users/Matt/Documents/GRAD SCHOOL/BUAN 5210/Final Project')
rm(list=ls(all=TRUE))
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)

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

##Starting the EDA

#The goal of this analysis is to explore the patterns in bike ridership and determine how to increase the number
#of users for the bikeshare program

#We will identify which factors cause an increase in ridership and recommend creating policies to maximize 
#ridership for that segment


#This dataset is larger than what I'm used to dealing with so I may try to analyze it in chunks
#Because the data is broken down into individual trips, let's look at a summary of trips by day and month

dataset$duration <- as.numeric(dataset$duration)
dataset$month <- format(as.Date(dataset$date), "%Y-%m")

daily <- dataset %>%
  select(date, duration) %>%
  group_by(date) %>%
  summarize(total_trips=n(), avg_duration=mean(duration))

monthly <- dataset %>%
  select(month, duration) %>%
  group_by(month) %>%
  summarize(total_trips=n(), avg_duration=mean(duration))

ggplot(daily, aes(x = date, y = total_trips)) +
  geom_bar(stat = "identity", position="dodge") 

ggplot(monthly, aes(x = month, y = total_trips)) +
  geom_line(group=1) +
  geom_point(size=2)

#As expected, there seems to be more riders in the summer than in the winter

#What about average ride length?
ggplot(daily, aes(x = date, y = avg_duration)) +
  geom_bar(stat = "identity", position="dodge") +
  ylim(0,10000)

#This seems to be more cyclical, perhaps there is connection between long rides and the weekend?

dataset$day <- weekdays(as.Date(dataset$date))

day <- dataset %>%
  select(date, duration, day) %>%
  group_by(day) %>%
  summarize(total_trips=n(), avg_duration=mean(duration))

#Indeed, we see that Saturday and Sunday have nearly 3 times the average ride length of weekdays
#Weekdays are likely filled with riders who are commuting, which is why there are so many more total trips
#Perhaps we should look into ways to improve ridership on the weekends?

#Is there a difference in popular days by city?

city_weekday <- dataset %>%
  select(day, city) %>%
  group_by(day, city) %>%
  summarize(total_trips=n())


ggplot(city_weekday, aes(x=city,y=total_trips, fill=day))+
  geom_bar(stat="identity",position="dodge") +
  xlab("City") +
  ylab("Trips")

#To see the smaller cities
ggplot(city_weekday, aes(x=city,y=total_trips, fill=day))+
  geom_bar(stat="identity",position="dodge") +
  xlab("City") +
  ylab("Trips") +
  ylim(0,5000)

#What about the distribution of duration? Are there a lot of really short rides?

ggplot(dataset, aes(x = duration/60)) +
  geom_histogram(binwidth = 5) +
  xlim(0,100)

#We can also look at the weather by day and determine how much of an impact that has on ridership

dataset$rainy_trip <- ifelse(dataset$events=="Fog-Rain"|dataset$events=="rain"|dataset$events=="Rain"|
                              dataset$events=="Rain-Thunderstorm",1,0)

weather_rides <- dataset %>%
  select(date, month, city, rainy_trip, mean_temperature_f) %>%
  group_by(date, month) %>%
  summarize(total_trips=n(),rainy_day=median(rainy_trip), avg_temp=mean(mean_temperature_f))

weather_months <- weather_rides %>%
  select(date, month, total_trips, rainy_day, avg_temp) %>%
  group_by(month) %>%
  summarize(total_trips=sum(total_trips),rainy_days=sum(rainy_day), avg_temp=mean(avg_temp))

#The number of trips seems to highly depend on the number of rainy days.

weather_avg <- weather_rides %>%
  select(date, total_trips, rainy_day) %>%
  group_by(rainy_day) %>%
  summarize(average_trips=sum(total_trips)/n())

#Rainy days have a small impact on ridership but not as much as expected
#There is not much we can do about rainy days anyway but we may come back to this later

#At the beginning, we had to merge the datasets based on zipcodes to get the weather data for each ride
#Let's look at the total distribution of rides by city 

#Because this dataset is very large, it might make sense to break the trips into city groups

Mountain_View <- dataset %>%
  filter(city=="Mountain View")

Palo_Alto <- dataset %>%
  filter(city=="Palo Alto")

Redwood_City <- dataset %>%
  filter(city=="Redwood City")

San_Francisco <- dataset %>%
  filter(city=="San Francisco")

San_Jose <- dataset %>%
  filter(city=="San Jose")

city_rides <- dataset %>%
  select(city) %>%
  group_by(city) %>%
  summarize(total_trips=n())

#Wow, it looks like the vast majority of the data (almost 90%) is coming from San Francisco

#Which city has the highest average ride duration?

city_duration <- dataset %>%
  select(city, duration) %>%
  group_by(city) %>%
  summarize(total_trips=n(), med_duration=median(duration))

#I tried this summary with both mean and median and got very different results
#This is expected because of the skewed distribution of the histogram, so median is probably better
#By this measure, the typical rider in Palo Alto will travel almost twice as long on average


city_duration <- dataset %>%
  select(city, duration) %>%
  group_by(city) %>%
  summarize(total_trips=n(), med_duration=median(duration)/60)

#The median ride is fairly short, but we did see a lot of trips on the longer end of the spectrum
#This is an important fact because riders are charged extra if their trip is longer than 30 mins

#What percent of total rides are over 30 minutes?

dataset$longride <- ifelse(dataset$duration>1800,1,0)
#35075/669959 = 0.052
#Only about 5% of rides are currently going over the 30 minute threshold

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

#However, there is a major spread in terms of which cities have a high percentage of longrides
#28% of rides in Palo Alto last longer than 30 minutes, which is way above any other city
#How can we get other cities to encourage these longer rides?

#Maybe elevation will provide more answers. I would think riders would be more inclined to
#use bike share if their journey is relatively flat

dataset$elevation_change <- dataset$start_station_elevation - dataset$end_station_elevation

elevation_city <- dataset %>%
  select(city, start_station_elevation, elevation_change) %>%
  group_by(city) %>%
  summarize(avg_start_elevation=mean(start_station_elevation), avg_elevation_change=mean(elevation_change))

#Riders in SF tend to have trips that are slightly uphill, but 2 feet of change seems manageable
#I did notice that many trips seemed to start and end at the same station, which would drag down the avg
#Let's make a separate group of trips that start and end at different stations

dataset$different_stations <- ifelse(dataset$start_station_id==dataset$end_station_id,0,1)

elevation_city <- dataset %>%
  filter(different_stations==1) %>%
  select(city, start_station_elevation, elevation_change) %>%
  group_by(city) %>%
  summarize(avg_start_elevation=mean(start_station_elevation), avg_elevation_change=mean(elevation_change))

#Still no numbers that jump out. Maybe once we analyze stations we can see how they are clustered

station_trips <- dataset %>%
  select(start_station_name, city, start_station_elevation) %>%
  group_by(start_station_name, city) %>%
  summarize(total_trips=n(), elevation=mean(start_station_elevation))

#Interesting distribution of trips between the stations, elevation does not seem to have much impact
#What if we add dock count? I would assume stations with higher dock counts would see more trips

station_trips <- dataset %>%
  select(start_station_name, city, dock_count, start_station_elevation) %>%
  group_by(start_station_name, city) %>%
  summarize(total_trips=n(), dock_count=mean(dock_count), elevation=mean(start_station_elevation))

#San Francisco has the vast majority of trips, let's plot trips against dock count

SF_trips <- dataset %>%
  filter(city=="San Francisco") %>%
  select(start_station_name, city, dock_count, start_station_elevation) %>%
  group_by(start_station_name, city) %>%
  summarize(total_trips=n(), dock_count=mean(dock_count), elevation=mean(start_station_elevation))

ggplot(SF_trips, aes(x = total_trips, y = dock_count)) +
  geom_point() +
  ylim(12,30)

#And the total with city indicated by color
ggplot(station_trips, aes(x = total_trips, y = dock_count, color=city)) +
  geom_point(aes(fill=city)) +
  ylim(12,30)

#Points towards the lower (low dock_count, high total_trips) should be examined because they
#likely would benefit from having more docs installed to encourage more rides
#SF Caltrain has the most total trips yet it only has 19 docks. It also has nearly 7 times more
#total_trips than the Golden Gate stop but GG has 23 docks.
#Finding an optimal distribution of stations and docks could improve overall ridership

#Finding a trips/dock figure for each station should give us a sense of which stations could
#benefit from having more docks

trips_per_dock <- station_trips %>%
  select(start_station_name, city, total_trips, dock_count) %>%
  group_by(start_station_name, city) %>%
  summarize(trips_per_dock=total_trips/dock_count)

#Created a summary of docks by city for the dashboard

#What about subscriber type? Knowing how many trips are by subscribers and how many are from regular
#customers could help us target new subscribers

#Let's check by city and also see how these numbers have changed over time

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

#Again, we see Palo Alto is the outlier. The other cities all see 15-20% of their trips from
#customers, while Palo Alto has 43% of their trips from customers

#How have these numbers changed over time?

monthly_subscription <- dataset %>%
  select(month, subscription_type) %>%
  group_by(month, subscription_type) %>%
  summarize(total_trips=n())

#There are definitely dips in the winter months, this might make a nice graph

ggplot(data=monthly_subscription, aes(x=month, y=total_trips)) +
  geom_point(aes(colour = subscription_type),size=2) +
  geom_line(aes(colour = subscription_type, group = subscription_type)) +
  xlab("Month") +
  ylab("Total Trips")
  
#How does temperature figure into the equation? 
#We would expect more trips as the temperature goes up (to a certain point)

temp_rides <- dataset %>%
  select(mean_temperature_f) %>%
  group_by(mean_temperature_f) %>%
  summarize(total_trips=n())

ggplot(temp_rides, aes(x = mean_temperature_f, y=total_trips)) +
  geom_bar(stat = "identity") +
  xlim(35,85)

#This is influenced by the number of days at each temperature, so it might not be telling us much
#What if we try by duration?

temp_rides <- dataset %>%
  select(mean_temperature_f, duration) %>%
  group_by(mean_temperature_f) %>%
  summarize(total_trips=n(), avg_duration=mean(duration))

ggplot(temp_rides, aes(x = mean_temperature_f, y=avg_duration)) +
  geom_bar(stat = "identity") +
  xlim(35,85)

#Looks like ride duration is not much affected by temperature
#The random spikes at the ends are due to temperature brackets with only a few trips
#I don't think wind will have much of an influence but let's see just in case 

wind_rides <- dataset %>%
  select(mean_wind_speed_mph, duration) %>%
  group_by(mean_wind_speed_mph) %>%
  summarize(total_trips=n(), avg_duration=mean(duration))

ggplot(wind_rides, aes(x = mean_wind_speed_mph, y=avg_duration)) +
  geom_bar(stat = "identity") +
  xlim(0,25)

#There is a little more variation and maybe a slight downward trend (indicating lower wind -> more rides)
#But overall not convincing or conclusive


#Leftover code

#combine city long percent and city subscription percent into one graph for dashboard

city_melt <- left_join(city_subscription_percent,city_long_percent, by = c("city"))

city_melt <- melt(city_melt)

ggplot(city_melt, aes(x=city,y=value, fill=variable))+
  geom_bar(stat="identity",position="dodge") +
  xlab("City") +
  ylab("Percentage") +
  scale_fill_discrete(name="Variable",
                      labels=c("Percentage\nof rides\nby customers\n", 
                               "Percentage\nof rides\nover 30 mins\n"))





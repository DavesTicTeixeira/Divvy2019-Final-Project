# Adding Library's 

library(tidyverse)
library(skimr)
library(hydroTSM)
library(lubridate)

# Add Data - Using read.csv
# Changed the file reading from .csv to .zip so it reads directly and works best with github a disk usage


Q1 <- read_csv("data/ZIP/Divvy_Trips_2019_Q1.zip", col_names = TRUE)
Q2 <- read_csv("data/ZIP/Divvy_Trips_2019_Q2.zip", col_names = TRUE)
Q3 <- read_csv("data/ZIP/Divvy_Trips_2019_Q3.zip", col_names = TRUE)
Q4 <- read_csv("data/ZIP/Divvy_Trips_2019_Q4.zip", col_names = TRUE)

# Correcting columns names from Q2 to match with others

col_names_list <-
  as.list(names(Q1)) # First create a list with the original col names

colnames(Q2) <-
  c(col_names_list) # Use the list to correct the col names in Q2

# Adding all data sets together in one using bind_rows

df <- bind_rows(Q1, Q2, Q3, Q4)

# Counting rows of Q data and df to make sure it all matches

sum(count(Q1), count(Q2), count(Q3), count(Q4))

count(df)

# It seems all data is accountable

remove(Q1,Q2,Q3,Q4,col_names_list) # Remove Data from Global Environment

# Let's make a copy of the original data

bike_trip <- df

# Save the bike_trip df in order to be easier to load data if restarted

# Don't Run 
# write_csv(bike_trip, "bike_trip.csv")

# Don't Run 
# bike_trip <- read.csv("bike_trip.csv") # Save time by loading the df in case R is restarted

# Get the trip duration in minutes

bike_trip$trip <- difftime(bike_trip$end_time, bike_trip$start_time)

bike_trip$trip <- round(bike_trip$trip) # Round to minutes

bike_trip$trip <- as.numeric(bike_trip$trip)

# Remove Original Trip_Duration column

bike_trip <- subset(bike_trip, select = -c(tripduration))

# Remove negative ride times

bike_trip <- 
  bike_trip %>% 
  filter(trip >= 0)

# Remove the lower and upper quartile based on IQR

bike_trip <- 
  bike_trip %>% 
  filter(trip <= (IQR(trip)*1.5) + (quantile(trip, 0.75))) %>% 
  filter(trip >= quantile(trip,0.25))

max(bike_trip$trip)
min(bike_trip$trip)

# Make new columns Weekday / Month / Hour

bike_trip <-
  bike_trip %>%
  mutate(
    month = month(start_time),
    weekday = weekdays(start_time),
    hour = hour(start_time)
  )

bike_trip$weekday <- 
  factor(bike_trip$weekday, 
         levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Reformat Hour 0:23 to 1:24

bike_trip$hour[bike_trip$hour == "0"] <- "24"

bike_trip$hour <- 
  factor(bike_trip$hour, 
         levels = c("1","2","3","4","5","6","7","8",
                    "9","10","11","12","13","14","15","16",
                    "17","18","19","20","21","22","23","24"))

# Adding Season with a different package

bike_trip$season <- time2season(bike_trip$start_time,
                            out.fmt = "seasons",
                            type = "default")

unique(bike_trip$season) # confirmation / Autumn is misspelling

bike_trip$season[bike_trip$season == "autumm"] <- "autumn" # Correction of the misspelling

bike_trip$season <- 
  factor(bike_trip$season, 
         levels = c("winter","spring","summer","autumn"))

# Add Date column

bike_trip$date <- as.Date(bike_trip$start_time)

# Stations

# Top 10 Stations Gen Pop Start and End

top_10_stations_popgen_start <-
    as.data.frame(table(
      bike_trip$from_station_name))

top_10_stations_popgen_start <- 
        top_10_stations_popgen_start %>% 
          rename( "Top 10 Starting Stations" = Var1,
                  "Rides" = Freq)

top_10_stations_popgen_end <-
  as.data.frame(table(
    bike_trip$to_station_name))

top_10_stations_popgen_end <- 
  top_10_stations_popgen_end %>% 
  rename( "Top 10 End Stations" = Var1,
          "Rides" = Freq)

# This filters Customer

top_10_stat_cust <- 
  bike_trip %>% 
  filter(str_detect
        (usertype, "Customer")) # Next table with stations use

top_10_stat_cust <-
  as.data.frame(table
               (top_10_stat_cust$from_station_name)) # Next table with stations use

top_10_stat_cust <-
  head(arrange
      (top_10_stat_cust, desc(Freq)), n = 10) # And finally the top 10 Per Customer

top_10_stat_cust <- 
  top_10_stat_cust %>% 
  rename( "Top 10 Starting Stations" = Var1, "Total Rides" = Freq)


   
# Time to start messing with the plots

# Annual Trend for total number of trips in 2019

day_tri_cum <- (as.data.frame(table(bike_trip$date)))

day_tri_cum$Var1 <- as.Date(day_tri_cum$Var1)

day_tri_cum %>% 
  ggplot(aes(x = Var1, y = Freq))+
  geom_point(alpha = 0)+
  geom_smooth(se = FALSE, method = "loess", formula = y~x)+
  labs(title = "Passenger Trend 2019", x = "", y = "Total Trips")+
  theme_bw()
 
# Avg and Mode Trip_Time for 2019

#Add mode function

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mean(bike_trip$trip)
getmode(bike_trip$trip)

# Subscribers Vs Customer 

subs_vs_cust <- 
  as.data.frame(table(bike_trip$usertype)) %>% 
  rename(Riders = Var1)


ggplot(subs_vs_cust, aes(x="", y=Freq, fill=Riders), color = "white") +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_manual(values=c("#b4def0", "#ff0019"))


# Subscribers behavior

#DF filtered for subscribers

bike_trip_subs <- 
  filter(bike_trip, usertype == "Subscriber")

bike_trip_subs$season <- 
  factor(bike_trip_subs$season, 
  levels = c("winter","spring","summer","autumn"))

bike_trip_subs$weekday <- 
  factor(bike_trip_subs$weekday, 
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

bike_trip_subs$hour <- 
  factor(bike_trip_subs$hour, 
  levels = c("1","2","4","5","6","7","8",
             "9","10","11","12","13","14","15","16",
             "17","18","19","20","21","22","23","24"))

# Create DF for season and plot

bike_trip_subs_season <-
  as.data.frame(table(bike_trip_subs$season))%>% 
  rename(seasons = Var1, Rides = Freq)

bike_trip_subs_season %>%
  ggplot(aes(x = seasons, y = Rides))+
  geom_col(fill = "#3db7e4")+
  theme_light()

# Subsetting for the summer months only and looking at the weekdays

bike_trip_subs_summer_weekdays <- 
  bike_trip_subs %>% filter(season == "summer")
    
bike_trip_subs_summer_weekdays <- 
  as.data.frame(table(bike_trip_subs_summer_weekdays$weekday)) %>% 
  rename(Days = Var1, Rides = Freq)

bike_trip_subs_summer_weekdays %>%
  ggplot(aes(x = Days, y = Rides))+
  geom_col(fill = "#3db7e4")+
  theme_light()

# Rides on Summer Months by hour on weekdays.

# This is by far the best approach so far.

  # Filtler out the desired conditions.

  bike_trip %>% 
  filter(usertype == "Subscriber",
         season == "summer",
         !weekday %in% c("Saturday", "Sunday")) %>%
  
  # And then Plot
  
  ggplot(aes(x = hour))+
  geom_bar(fill = "#3db7e4")+
  theme_light()

  # Repeat with Tuesday only

  bike_trip %>% 
  filter(usertype == "Subscriber",
         season == "summer",
         weekday == "Tuesday") %>%
    
  # And then Plot
    
  ggplot(aes(x = hour))+
  geom_bar(fill = "#3db7e4")+
  labs(title = "Rides on Tuesdays Summer Months for Subscribers",
       x = "Hour of the Day",
       y = "Total Rides")+
  theme_bw()

  # Use the same logic to get the mean trip.

  bike_trip %>% 
  filter(usertype == "Subscriber",
         season == "summer",
         weekday == "Tuesday") %>%
  summarise(mean = mean(trip))
  
# -- A Note before moving forward -- 
# I should probably go back and change all the code that is behind
# the logic (mine) for not doing it, is it only to show and see how far did I came
# from the beginning of this project. And for me that is important.

# Looking at Casual Riders
  
  # By Season
  
  bike_trip %>% 
    filter(usertype == "Customer") %>%
    
    ggplot(aes(x = season))+
    geom_bar(fill = "#3db7e4")+
    labs(title = "Rides on Tuesdays Summer Months for Subscribers",
         x = "Hour of the Day",
         y = "Total Rides")+
    theme_bw()
  
  # By weekday in Summer
  
  bike_trip %>% 
    filter(usertype == "Customer",
           season == "summer") %>%
    
    ggplot(aes(x = weekday))+
    geom_bar(fill = "#3db7e4")+
    labs(title = "Rides on Tuesdays Summer Months for Subscribers",
         x = "Hour of the Day",
         y = "Total Rides")+
    theme_bw()
  
  # By hour on weekends and on Saturday Summer season
  
  # Weekends
  
  bike_trip %>% 
    filter(usertype == "Customer",
           season == "summer",
           weekday %in% c("Saturday", "Sunday")) %>%
    
    ggplot(aes(x = hour))+
    geom_bar(fill = "#3db7e4")+
    labs(title = "Rides on Tuesdays Summer Months for Subscribers",
         x = "Hour of the Day",
         y = "Total Rides")+
    theme_bw()

    # Saturday
  
  bike_trip %>% 
    filter(usertype == "Customer",
           season == "summer",
           weekday == "Saturday") %>%
    
    ggplot(aes(x = hour))+
    geom_bar(fill = "#3db7e4")+
    labs(title = "Rides on Tuesdays Summer Months for Subscribers",
         x = "Hour of the Day",
         y = "Total Rides")+
    theme_bw()

  # Weekdays
  
  bike_trip %>% 
    filter(usertype == "Customer",
           season == "summer",
           !weekday %in% c("Saturday", "Sunday")) %>%
    
    ggplot(aes(x = hour))+
    geom_bar(fill = "#3db7e4")+
    labs(title = "Rides on Tuesdays Summer Months for Subscribers",
         x = "Hour of the Day",
         y = "Total Rides")+
    theme_bw()
  
  #END


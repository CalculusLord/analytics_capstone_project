library(tidyverse)
library(lubridate)
library(ggplot2)

# importing data
may_2022 <- read.csv("~/Desktop/capstone_project/data_csv_files/202205-divvy-tripdata.csv")
jun_2022 <- read.csv("~/Desktop/capstone_project/data_csv_files/202206-divvy-tripdata.csv")
jul_2022 <- read.csv("~/Desktop/capstone_project/data_csv_files/202207-divvy-tripdata.csv")
aug_2022 <- read.csv("~/Desktop/capstone_project/data_csv_files/202208-divvy-tripdata.csv")
sep_2022 <- read.csv("~/Desktop/capstone_project/data_csv_files/202209-divvy-publictripdata.csv")
oct_2022 <- read.csv("~/Desktop/capstone_project/data_csv_files/202210-divvy-tripdata.csv")
nov_2022 <- read.csv("~/Desktop/capstone_project/data_csv_files/202211-divvy-tripdata.csv")
dec_2022 <- read.csv("~/Desktop/capstone_project/data_csv_files/202212-divvy-tripdata.csv")
jan_2023 <- read.csv("~/Desktop/capstone_project/data_csv_files/202301-divvy-tripdata.csv")
feb_2023 <- read.csv("~/Desktop/capstone_project/data_csv_files/202302-divvy-tripdata.csv")
mar_2023 <- read.csv("~/Desktop/capstone_project/data_csv_files/202303-divvy-tripdata.csv")
apr_2023 <- read.csv("~/Desktop/capstone_project/data_csv_files/202304-divvy-tripdata.csv")

# consolidating tables
all_trips <- bind_rows(may_2022, jun_2022, jul_2022, aug_2022, sep_2022, oct_2022, nov_2022,
                       dec_2022, jan_2023, feb_2023, mar_2023, apr_2023)

# deleting unnecessary rows
all_trips <- all_trips %>% select(-c(start_lat, start_lng, end_lat, end_lng))

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

# Removing bad data
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)


all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week,
                                    levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Analyze rider data by type and weekday
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration
    ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)

# Visualizing by number of rides and rider type
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
    ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)  %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Visualizing average duration for rides
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
    ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)  %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

# Outputting csv
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '~/Desktop/capstone_project/avg_ride_length.csv')

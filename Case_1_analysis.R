#Libraries
library(tidyverse)
library(ggplot2)
library(skimr)
library(janitor)
library(lubridate)
library(readr)
library(scales)
library(conflicted)
library(stringr)

sink("./Rstudio/Case Studies/output.txt") #Used to redirect output to file
# Get the a list of the files conatining the monthly datasets
# combine them into a yearly aggregate
csv_files <- list.files(path="./Rstudio/Case Studies/data/case 1/", pattern = "\\.csv$", full.names = TRUE ,recursive = TRUE)
data_list <- lapply(csv_files, read.csv)
aggregated_tibble <- as_tibble(do.call(rbind, data_list))


# Inspect the new table that has been created

colnames(aggregated_tibble)
str(aggregated_tibble)
glimpse(aggregated_tibble)


#skim_without_charts(aggregated_tibble) # Shorcut for some descriptive analysis
summary(aggregated_tibble)

nrow(aggregated_tibble)
dim(aggregated_tibble)

head(aggregated_tibble) # Or tail()

# Change data type of "started_at" and the "ended_at" attributes to Date
# Add an additional calculated field for the trip durations

aggregated_tibble_clean <- aggregated_tibble %>% 
  select(-c(start_lat, start_lng , end_lat, end_lng)) %>%  # Remove some columns  
  mutate(started_at = ymd_hms(started_at)) %>% 
  mutate(ended_at = ymd_hms(ended_at)) %>% 
  mutate(trip_duration = as.duration(ended_at - started_at)) %>% 
  mutate(day_of_week = wday(started_at, label = TRUE, abbr = FALSE)) %>%  # Add day_of_week variable
  mutate(year_month = as.Date(paste(year(started_at), month(started_at), "01", sep = "-"))) # Add an additional Date for monthly grouping

glimpse(aggregated_tibble_clean)
skim_without_charts(aggregated_tibble_clean) 
# Trip duration stats are questionable (< 0 min and unreasonably high max duration)

#Find out how many empty strings are there in the start and end station ids and names
# OR USE skim_without_charts!!!!
conflicts_prefer(dplyr::filter())

empty_start_ids <- aggregated_tibble_clean %>%
  select(c(start_station_id)) %>%
  filter(start_station_id == "") %>%
  summarise(empty_strings = n())

empty_start_names <- aggregated_tibble_clean %>%
  select(c(start_station_name)) %>%
  filter(start_station_name == "") %>%
  summarise(empty_strings = n())

empty_end_id <- aggregated_tibble_clean %>%
  select(c(end_station_id)) %>%
  filter(end_station_id == "") %>%
  summarise(empty_strings = n())

empty_end_names <- aggregated_tibble_clean %>%
  select(c(end_station_name)) %>%
  filter(end_station_name == "") %>%
  summarise(empty_strings = n())

empty_strings <- bind_rows(empty_start_ids, empty_start_names, empty_end_id, empty_end_names)
empty_strings <-rownames_to_column(empty_strings, var = "row_name")
empty_strings$row_name <- c("empty_start_ids", "empty_start_names", "empty_end_id", "empty_end_names")
print(empty_strings)
#Limitation: there are many observations(rows) with empty ids and names for start stations and end stations
#This would make it difficult for further analysis into stations that member or casual riders frequently use.

#Find out why timespan/time_duration is way too high (max) or below 0 (min)
max_duration <- 5*60*60 #Arbitrarly chosen max duation (5 hrs max seems to be the most reasonable amount of time to rent a bike)
high_durations <- aggregated_tibble_clean %>%
  select(c(trip_duration)) %>%
  filter(trip_duration > max_duration) %>%
  summarise(high_durations = n())

negative_durations <- aggregated_tibble_clean %>%
  select(c(trip_duration)) %>%
  filter(trip_duration < 0) %>%
  summarise(high_durations = n())

unreasonable_durations <- bind_rows(high_durations, negative_durations)
unreasonable_durations <- rownames_to_column(unreasonable_durations, var = "row_name")
unreasonable_durations$row_name <- c("high_durations", "negative_durations")
print(unreasonable_durations)

#Remove all negative durations and keep unreasonably high trip durations (need more context to decide on what durations to keep)
aggregated_tibble_clean <- aggregated_tibble_clean %>% 
  filter(trip_duration >= 5) # Keep trips that are 5 minutes or more
summary(aggregated_tibble_clean$trip_duration)
#Limitation: There are trips with unreasonably high trip durations (> ~9 weeks). Will need more context
# to understand why and whether to remove some of the unreasonably high trip durations. 

# Analysis
# For The analysis, we will be investigating a variety of questions:
# 1.  How many members and casual riders are there for each month? 
#       Limitation: Cannot Get this info since personal identifying info is not provided
# Year
# 2.  What is the total number of trips for each user category for the whole year (08/2023 - 08/2024)?
# 3.  What is the average duration of trips taken by each user category?
# 4.  What rideable type do each user type prefer?
# Month
# 5.  What is the total monthly number of trips for each user category?
# 6.  What is the average monthly trip duration for each user category?
# 7.  What rideable type do each user type prefer?
# Day
# 8.  What is the total number of trips for each user category?
# 9.  What is the average daily trip duration for each user category?
# 10. What rideable type do each user type prefer?

#Perform some aggregate descriptive analysis on trip durations for each user type
aggregate(aggregated_tibble_clean$trip_duration ~ aggregated_tibble_clean$member_casual, FUN = mean)
aggregate(aggregated_tibble_clean$trip_duration ~ aggregated_tibble_clean$member_casual, FUN = median)
aggregate(aggregated_tibble_clean$trip_duration ~ aggregated_tibble_clean$member_casual, FUN = max)
aggregate(aggregated_tibble_clean$trip_duration ~ aggregated_tibble_clean$member_casual, FUN = min)

# Average daily ride duration
aggregate(aggregated_tibble_clean$trip_duration ~ aggregated_tibble_clean$day_of_week, FUN = mean)

# Average daily ride duration for each user type for each day
avg_daily_duration <- aggregate(aggregated_tibble_clean$trip_duration ~ aggregated_tibble_clean$member_casual + aggregated_tibble_clean$day_of_week, FUN = mean)
avg_daily_duration[order(avg_daily_duration$`aggregated_tibble_clean$member_casual`),]
#Casual riders have higher average trip duration every day of the week compared to members
      # OR
# Number of rides and average duration per user type for each day
aggregated_tibble_clean %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), average_duration = mean(trip_duration)) %>%
  arrange(member_casual, day_of_week)	

sink() # close output redirection 
sink.number() # check number of sink instances

#Plotting

#Useful function
grouping <- function(data){
  data %>% 
    select(
      -c(
        start_station_name,
        start_station_id,
        end_station_name,
        end_station_id,
        rideable_type
      )) %>% 
    group_by(member_casual)
}

remove_columns <- function(df){
  df %>% 
    select(
      -c(
        start_station_name,
        start_station_id,
        end_station_name,
        end_station_id,
        rideable_type
      ))
}

labelling <- function(title, x, y, variable){
  labs(
    title = title,
    subtitle = "(08/2023 - 08/2024)",
    x = x,
    y = y,
    caption = "Divvy Trip Data: https://divvy-tripdata.s3.amazonaws.com/index.html",
    fill = variable,
    color = variable
  )
}

# Year

# Total trips taken by each user category
grouping(aggregated_tibble_clean) %>%
  summarise(year_count = n()) %>%
  ggplot(aes(x = member_casual, y = year_count, fill = member_casual)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = comma(year_count)),
            vjust = -0.3,
            size = 3.5) +
  labelling("Total Yearly Trips: Casuals Vs. Members", "User Category", "Trips", "User Type") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)
ggsave(filename = "total_trips_per_user.png", path = "./Rstudio/Case Studies/Case_1_plots/", width = 5, height = 4)

# Total Average Duration of trips taken by each user category?
grouping(aggregated_tibble_clean) %>%
  summarise(trip_avrg = as.integer(mean((trip_duration)))) %>%
  mutate(formatted_trip_avrg = sprintf("%dm %02ds", trip_avrg %/% 60, trip_avrg %% 60)) %>%
  ggplot(aes(x = member_casual, y = trip_avrg, fill = member_casual)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = formatted_trip_avrg),
            vjust = -0.3,
            size = 3.5) +
  labelling("Yearly Average Trip Duration: Casuals Vs. Members", "User Category", "Duration", "User Type") +
  theme(legend.position = "none") +
  scale_y_time()
ggsave(filename = "total_avrg_durations_user.png", path = "./Rstudio/Case Studies/Case_1_plots/", width = 5, height = 4)

# Rideable type do each user type prefer 
aggregated_tibble_clean %>%
  select(c(ride_id, rideable_type, member_casual)) %>%
  group_by(member_casual, rideable_type) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = rideable_type, y = count, fill = member_casual)) +
  geom_bar(stat = "identity") +
  labelling("Yearly Bike Type Preference", "Bike Type", "Trips", "User Type") +
  theme(legend.position = "none") +
  facet_wrap( ~ member_casual) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(
    labels = function(x)
      str_replace_all(x, "_", " ")
  ) +
  scale_y_continuous(labels = comma)
ggsave(filename = "rideable_type_yearly_user.png", path = "./Rstudio/Case Studies/Case_1_plots/", width = 10, height = 4)

# Month (08/2023 - 08/2024)

# Total trips per user type by month 
aggregated_tibble_clean %>%
  remove_columns() %>%
  group_by(member_casual, year_month) %>%
  summarise(monthly_count_field = n()) %>%
  ggplot(aes(x = year_month, y = monthly_count_field, colour = member_casual)) +
  geom_line(stat = "identity") +
  labelling("Total Monthly Trips: Casuals Vs. Members", "Date", "Trips", "User Type") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma)
ggsave(filename = "total_trips_user_monthly.png", path = "./Rstudio/Case Studies/Case_1_plots/", width = 5, height = 4)

# Average monthly trip duration for each user type
aggregated_tibble_clean %>%
  remove_columns() %>%
  group_by(member_casual, year_month) %>%
  summarise(avrg_duration = mean(trip_duration)) %>%
  ggplot(aes(x = year_month, y = avrg_duration, colour = member_casual)) +
  geom_line(stat = "identity") +
  labelling("Average Monthly Trip Duration: Casuals Vs. Members", "Date", "Duration", "User Type") +
  theme(legend.position = "top") +
  scale_y_time()
ggsave(filename = "avrg_monthly_durations_user.png", path = "./Rstudio/Case Studies/Case_1_plots/", width = 7, height = 4)

# Rideable type do each user type prefer 
aggregated_tibble_clean %>%
  select(c(ride_id, rideable_type, member_casual, year_month)) %>%
  group_by(member_casual, year_month, rideable_type) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = rideable_type, y = count, fill = member_casual)) +
  geom_bar(stat = "identity") +
  labelling("Monthly Bike Type Preference", "Bike Type", "Trips", "User Type") +
  theme(legend.position = "top") +
  facet_wrap(~ year_month) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(
    labels = function(x)
      str_replace_all(x, "_", " ")
  ) +
  facet_wrap(~year_month) + 
  scale_y_continuous(labels = comma)
ggsave(filename = "rideable_type_monthly_user.png", path = "./Rstudio/Case Studies/Case_1_plots/", width = 10, height = 10)

# Day

# Total number of daily trips per user type
aggregated_tibble_clean %>% 
  remove_columns() %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labelling("Total Daily Trips: Casuals Vs. Members", "Day", "Trips", "User Type") +
  theme(legend.position = "top") +
  scale_y_continuous(labels = comma)
ggsave(filename = "total_daily_trips_user.png", path = "./Rstudio/Case Studies/Case_1_plots/", width = 5, height = 4)

# Average daily trip durations per user type
aggregated_tibble_clean %>% 
  remove_columns() %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), average_duration = mean(trip_duration)) %>%
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labelling("Average Daily Trip Durations: Casuals Vs. Members", "Day", "Duration", "User Type") +
  theme(legend.position = "top") +
  scale_y_time()
ggsave(filename = "avrg_daily_trip_durations_user.png", path = "./Rstudio/Case Studies/Case_1_plots/", width = 7, height = 4)

# Rideable type do each user type prefer 
aggregated_tibble_clean %>%
  select(c(ride_id, rideable_type, member_casual, day_of_week)) %>%
  group_by(member_casual, day_of_week, rideable_type) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = rideable_type, y = count, fill = member_casual)) +
  geom_bar(stat = "identity") +
  labelling("Daily Bike Type Preference: Casuals Vs. Members", "Bike Type", "Trips", "User Type") +
  theme(legend.position = "none") +
  facet_wrap( ~ member_casual) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_x_discrete(
    labels = function(x)
      str_replace_all(x, "_", " ")
  ) +
  facet_wrap(~day_of_week) +
  scale_y_continuous(labels = comma)
ggsave(filename = "rideable_type_daily_user.png", path = "./Rstudio/Case Studies/Case_1_plots/", width = 10, height = 10)

#Writing some stats into csv to visualize them using other tools (ex. spreadsheets or Tableau)
daily_trip_duration <- aggregate(aggregated_tibble_clean$trip_duration ~ aggregated_tibble_clean$member_casual + aggregated_tibble_clean$day_of_week, FUN = mean)
colnames(daily_trip_duration) <- c( "User", "Day", "Duration")
write.csv(daily_trip_duration, , file = './Rstudio/Case Studies/data/avg_daily_ride_duration.csv')
  

  




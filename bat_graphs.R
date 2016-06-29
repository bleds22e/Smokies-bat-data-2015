# Plotting Bat Data
# EKB
# 6/16/2016

######################
# LIBRARIES

library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)

######################
# FUNCTIONS

get_bat_data <- function(file){
  # tidy up the dataframe for a bat
  batdat <- file %>% 
            tidyr::unite(old_date_time, date, time, sep = " ") %>% 
            filter(skin_temp > 0 & skin_temp < 100, time_of_day == 'Day')
  parsed_bat <- as.data.frame(parse_date_time(batdat$old_date_time, "mdy_hms", tz = "EST"))
  colnames(parsed_bat) <- c("date_time")
  batdat <- cbind(batdat, parsed_bat) %>% 
            select(bat_id, date_time, skin_temp, time_of_day, sex, repro, age, species) %>% 
            arrange(date_time)
  return(batdat)
}

get_temp_2015 <- function(bat, weather){
  # function to add bat id to 2015 weather
  weather <- weather %>% 
             tidyr::separate(date_time, c("date", "time"), sep = " ")
  bat <- bat %>% 
         tidyr::separate(date_time, c("date", "time"), sep = " ")
  bat_weather <- semi_join(weather, bat, by = 'date')
  bat_weather$bat_id <- bat$bat_id[2]
  bat_weather <- bat_weather %>% tidyr::unite(date_time, date, time, sep = " ")
  as.POSIXct(bat_weather$date_time, tz = "EST")
  return(bat_weather)
}

get_weather <- function(bat){
  # get the appropriate weather data for each bat
  if (year(bat$date_time[2]) == '2014'){
    bat_weather <- weather2014 %>% filter(weather2014$bat_id == bat$bat_id[2])
  } else {
    get_temp_2015(bat, weather2015)
  }
} 

plot_torpor <- function(bat, weather){
  # plot bat temp, outside temp, light and deep torpor over time for a bat
  ggplot(bat, aes(x = date_time, y = skin_temp)) +
    geom_point()+
    geom_point(data = weather, aes(x = date_time, y = temp_C), color = "red")+
    geom_hline(aes(yintercept=25, color="red", linetype="dashed"))+
    geom_hline(aes(yintercept=10, color="red", linetype="dashed"))+
    scale_x_datetime(date_breaks = "1 day") +
    theme_bw()
}

######################
# LOAD FILES

# 2014 Weather
weather2014 <- read.csv("barney_data/hobo_data_by_bat.csv") %>% 
               select(bat = Bat, date_time = PASTE.VALUES, temp_C = Temperature...C..c.1) 
parsed_2014 <- as.data.frame(parse_date_time(weather2014$date_time, "mdy_hms", tz = "EST"))
weather2014 <- cbind(weather2014, parsed_2014)
colnames(weather2014) <- c("bat_id", "old_date_time", "temp_C", "date_time")
weather2014 <- select(weather2014, date_time, temp_C, bat_id) %>% arrange(date_time)

# 2015 Weather
weather2015 <- read.csv("barney_data/weather_2015.csv") %>% 
               rename(temp_C = outside_temp)
parsed_2015 <- as.data.frame(parse_date_time(weather2015$date_time, "mdy_hm", tz = "EST"))
weather2015 <- cbind(weather2015, parsed_2015)
colnames(weather2015) <- c("old_date_time", "temp_C", "date_time")
weather2015 <- select(weather2015, date_time, temp_C) %>% arrange(date_time)

# bat data
filenames <- list.files(path = "barney_data", pattern = "bat_", full.names = FALSE)
bat_data <- list()
for (file in filenames){
  # make each bat a dataframe in a list called bat_data
  name <- gsub(".csv", "", file)
  bat_data[[file]] <- assign(name, get_bat_data(read.csv(paste("barney_data/", file, sep = ""), colClasses = c(sex = "character"))))
}

# weather data
bat_data_remove <- bat_data[-c(10, 11, 19)]

######################
# WORK AREA

# trying to get naming correct
weather_matched <- list()
bat_files <- names(bat_data_remove)
for (bat in bat_files){
  name <- gsub("bat", "weather", bat_file)
  name2 <- gsub(".csv", "", name)
  weather_matched[[bat]] <- assign(name2, get_weather(bat))
}



names(bat_data)


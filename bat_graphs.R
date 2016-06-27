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
# LOAD FILES

# 2014 Weather
weather2014 <- read.csv("barney_data/hobo_data_by_bat.csv") %>% 
               select(bat = Bat, date_time = PASTE.VALUES, temp_C = Temperature...C..c.1) 
parsed_2014 <- as.data.frame(parse_date_time(weather2014$date_time, "mdy_hms", tz = "EST"))
weather2014 <- cbind(weather2014, parsed_2014)
colnames(weather2014) <- c("bat", "old_date_time", "temp_C", "date_time")
weather2014 <- select(weather2014, date_time, temp_C, bat)

# 2015 Weather
weather2015 <- read.csv("barney_data/weather_2015.csv") %>% 
               rename(temp_C = outside_temp)
parsed_2015 <- as.data.frame(parse_date_time(weather2015$date_time, "mdy_hm", tz = "EST"))
weather2015 <- cbind(weather2015, parsed_2015)
colnames(weather2015) <- c("old_date_time", "temp_C", "date_time")
weather2015 <- select(weather2015, date_time, temp_C)

# bat data
filenames <- list.files(path = "barney_data", pattern = "bat_", full.names = FALSE)
for (file in filenames){
  name <- gsub(".csv", "", file)
  assign(name, read.csv(paste("barney_data/", file, sep = "")))
}

######################
# FUNCTIONS

get_bat_data <- function(filename){
  batdat <- read.csv(file = filename) %>% 
    tidyr::unite(old_date_time, date, time, sep = " ") %>% 
    filter(skin_temp > 0 & skin_temp < 100, time_of_day == 'Day')
  parsed_bat <- as.data.frame(parse_date_time(batdat$old_date_time, "mdy_hms", tz = "EST"))
  colnames(parsed_bat) <- c("date_time")
  batdat <- cbind(batdat, parsed_bat) %>% 
            select(bat_id, date_time, skin_temp, time_of_day, sex, repro, age, species)
  return(batdat)
}

plot_torpor <- function(bat, weather){
  ggplot(bat, aes(x = date_time, y = skin_temp)) +
    geom_point()+
    geom_point(data = weather, aes(x = date_time, y = temp_C), color = "red")+
    geom_hline(aes(yintercept=25, color="red", linetype="dashed"))+
    geom_hline(aes(yintercept=10, color="red", linetype="dashed"))+
    scale_x_datetime(date_breaks = "1 day") +
    theme_bw()
}

######################
# WORK AREA








# 150.073
bat_150.073 <- get_bat_data("barney_data/bat_150.073.csv")
weather_073 <- weather2014 %>% filter(bat == '150.073')

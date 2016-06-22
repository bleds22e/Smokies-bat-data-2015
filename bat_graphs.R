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

######################
# FUNCTIONS



######################
# WORK AREA

get_bat_data <- function(filename){
  batdat <- read.csv(file = filename) %>% 
    tidyr::unite(date_time, date, time, sep = " ")
  parsed_bat <- as.data.frame(parse_date_time(batdat$date_time, "mdy_hms", tz = "EST"))
  batdat <- cbind(batdat, parsed_bat) %>% 
    select(bat_id, date_time, skin_temp, time_of_day, sex, repro, age, species)
  return(batdat)
}

filenames <- list.files(path = "barney_data", pattern = "bat_")
filenames <- filenames[1:3]
for (file in filenames){
  print(file)
  get_bat_data(file)
}

get_bat_data("barney_data/bat_150.112.csv")

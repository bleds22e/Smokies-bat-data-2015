# Plotting Bat Data
# EKB
# 1/11/2017

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
  batdat <- read.csv(file, header = TRUE, colClasses = c(sex = "character"))
  batdat <- batdat %>% 
            tidyr::unite(old_date_time, date, time, sep = " ") %>% 
            filter(skin_temp > 0 & skin_temp < 100, time_of_day == 'Day')
  parsed_bat <- as.data.frame(parse_date_time(batdat$old_date_time, "mdy_hms", tz = "EST"))
  colnames(parsed_bat) <- c("date_time")
  batdat <- cbind(batdat, parsed_bat) %>% 
            select(bat_id, date_time, skin_temp, time_of_day, sex, repro, age, species) %>% 
            arrange(date_time)
  return(batdat)
}

load_bat_data <- function(files){
  # read, clean and bind all bat CSV files into one dataframe
  cleaned <- lapply(files, get_bat_data)
  bat_data <- bind_rows(cleaned)
  return(bat_data)
}

get_temp_2015 <- function(bat){
  # function to add bat id to 2015 weather
  weather <- weather2015 %>% 
             tidyr::separate(date_time, c("date", "time"), sep = " ")
  bat <- bat %>% 
         tidyr::separate(date_time, c("date", "time"), sep = " ")
  bat_weather <- semi_join(weather, bat, by = 'date')
  bat_weather$bat_id <- bat$bat_id[2]
  bat_weather <- bat_weather %>% tidyr::unite(date_time, date, time, sep = " ")
  return(bat_weather)
}

get_temp_2014 <- function(bat){
  weather <- weather2014 %>% 
             tidyr::separate(date_time, c("date", "time"), sep = " ") %>% 
             tidyr::unite(date_time, date, time, sep = " ") %>% 
             filter(weather2014$bat_id == bat$bat_id[2])
  bat$bat_id <- as.numeric(as.character(bat$bat_id))  
  return(weather)
}

get_weather <- function(bat){
  # get the appropriate weather data for each bat
  bat <- get_bat_data(bat)
  if (year(bat$date_time[2]) == '2014'){
    bat_weather <- get_temp_2014(bat)
  } else {
    bat_weather <- get_temp_2015(bat)
  }
  bat_weather$bat_id <- as.numeric(as.character(bat_weather$bat_id))
  return(bat_weather)
}  

load_weather_data <- function(files){
  # make one dataframe with all weather data labelled with appropriate bat ID 
  temps <- lapply(files, get_weather)
  weather_data <- bind_rows(temps)
  return(weather_data)
} 

plot_torpor <- function(bat, weather){
  # plot bat temp, outside temp, light and deep torpor over time for a bat
  bat_plot <- ggplot(bat, aes(x = date_time, y = runmed(skin_temp, 11))) +
    geom_point(size = 1)+
    geom_point(data = weather, aes(x = date_time, y = temp_C), color = "red", size = 1)+
    geom_hline(aes(yintercept=25), color="light blue", size = 1)+
    geom_hline(aes(yintercept=10), color="light blue", size = 1)+
    scale_x_datetime(date_breaks = "1 day") +
    ylab("Temp (C)") +
    xlab("Time")+
    ggtitle(bat$bat_id[2])+
    theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))
  print(bat_plot)
  bat_id <- gsub(".", "_", bat$bat_id[2], fixed = TRUE)
  ggsave(filename = paste("plots/", "plot", bat_id, sep = "", ".png"), plot = bat_plot)
}

######################
# LOAD FILES

### clean weather files

# 2014
weather2014 <- read.csv("barney_data/hobo_data_by_bat.csv") %>% 
               select(bat = Bat, date_time = PASTE.VALUES, temp_C = Temperature...C..c.1) 
parsed_2014 <- as.data.frame(parse_date_time(weather2014$date_time, "mdy_HMS", tz = "EST"))
weather2014 <- cbind(weather2014, parsed_2014)
colnames(weather2014) <- c("bat_id", "old_date_time", "temp_C", "date_time")
weather2014 <- select(weather2014, date_time, temp_C, bat_id) %>% arrange(date_time)

# 2015
weather2015 <- read.csv("barney_data/weather_2015.csv") %>% select(date_time, temp_C)
parsed_2015 <- as.data.frame(parse_date_time(weather2015$date_time, "mdy_HM", tz = "EST"))
weather2015 <- cbind(weather2015, parsed_2015)
colnames(weather2015) <- c("old_date_time", "temp_C", "date_time")
weather2015 <- select(weather2015, date_time, temp_C) %>% arrange(date_time)

### all files together

# bats

filenames <- list.files(path = "barney_data", pattern = "bat_", full.names = TRUE)
bat_data <- load_bat_data(filenames)

# weather
no_temp_bats_removed <- filenames[-c(2, 3, 10, 11, 19)]
weather_data <- load_weather_data(no_temp_bats_removed)
weather_data$date_time <- as.POSIXct(weather_data$date_time)

### plot by unique bat ID

# get unique ids
unique_id_temp <- select(weather_data, bat_id) %>% distinct()
unique_id_total <- select(bat_data, bat_id) %>% distinct()

# make plots per bat

for (bat in unique_id_temp$bat_id) {
  plot_torpor(filter(bat_data, bat_id == bat), filter(weather_data, bat_id == bat))
}

### list of unique bats and characteristics

bat_data[!duplicated(bat_data$bat_id), c(1,5:8)]

######################
# TEST CODE

test_bat <- bat_data %>% filter(bat_id == "151.23")
test_weather <- weather_data %>% filter(bat_id == "151.23")
plot_torpor(test_bat, test_weather)

######################
# WORK AREA

ggplot(test_bat, aes(x = date_time, y = runmed(skin_temp, 11))) +
  geom_point(size = 1)+
  geom_point(data = test_weather, aes(x = date_time, y = temp_C), color = "red", size = 1)+
  geom_hline(aes(yintercept=25), color="light blue", size = 1)+
  geom_hline(aes(yintercept=10), color="light blue", size = 1)+
  scale_x_datetime(date_breaks = "1 day") +
  ylab("Temp (C)") +
  xlab("Time")+
  ggtitle(test_bat$bat_id[2])+
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 15)))
  
  


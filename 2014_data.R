#2014 Smokies Bat Data
  # includes day-time data logger data, body temp 
  # and outside temps
# Ellen Bledsoe
# started April 4, 2016


# LOAD LIBRARIES 
####################################

library(dplyr)


# LOAD FILES
####################################

hobo_data <- read.csv("2014bats/2014_hobo_data.csv") 
myse_073 <- read.csv("2014bats/myse_073.csv")
myse_189 <- read.csv("2014bats/myse_189.csv")
myse_227 <- read.csv("2014bats/myse_227.csv")
myse_456 <- read.csv("2014bats/myse_456.csv")
myso_313 <- read.csv("2014bats/myso_313.csv")


# ARRANGE THE DATA
####################################

myse_073 <- select(myse_073, )

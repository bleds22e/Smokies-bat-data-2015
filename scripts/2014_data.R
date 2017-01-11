#2014 Smokies Bat Data
  # includes day-time data logger data, body temp 
  # and outside temps
# Ellen Bledsoe
# started April 4, 2016


# LOAD LIBRARIES 
####################################

library(dplyr)
library(ggplot2)

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

# sex -- male = 0, female = 1
# age -- juv = 0, adult = 1
# repro -- NR = 0, repro = 1
# species -- msye = 0, myso = 1

myse_227$Sex <- "1"
myse_227$Age <- "1"
myse_227$Repro <- "1"
myse_227$Species <- "0"

myso_313$Sex <- "0"
myso_313$Age <- "1"
myso_313$Repro <- "0"
myso_313$Species <- "1"

myse_456$Sex <- "1"
myse_456$Age <- "0"
myse_456$Repro <- "0"
myse_456$Species <- "0"

myse_073$Sex <- "1"
myse_073$Age <- "1"
myse_073$Repro <- "1"
myse_073$Species <- "0"

my
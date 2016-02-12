# Clean Smokies Bat Data #
# Ellen Bledsoe
# December 2015

##### Load File #####

# figure out some way to load all
files <-  list.files("data_csv", pattern="*.csv", full.names = TRUE)

myse_0937 <- read.csv("data_csv/MYSE_150937.csv")
myse_1909 <- read.csv("data_csv/MYSE_151909.csv")
myso_0995 <- read.csv("data_csv/MYSO_150995.csv")
myse_1230 <- read.csv("data_csv/MYSE_151230.csv")
myse_1312 <- read.csv("data_csv/MYSE_151312.csv")
myso_1547 <- read.csv("data_csv/MYSO_151547.csv")
myse_1467 <- read.csv("data_csv/MYSE_151467.csv")
myse_1830 <- read.csv("data_csv/MYSE_151830.csv")
myso_1786 <- read.csv("data_csv/MYSO_151786.csv")

##### Functions #####

check_channel <- function(data, channel){
  # checks for incorrect channels in the data
  for (n in 1:length(data[,1])) {
    if (data$Channel[n] != channel) {
      print(data$ref[n])
    }
  }
  print("done")
}

check_NA_timechange<- function(data){
  # checks for NAs in the time change column
  for (n in 1:length(data[,1])) {
    if (is.na(data$Time.Change[n])) {
      print(data$ref[n])
    }
  }
  print("done")
}

check_NA_time <- function(data){
  # checks for NAs in the time change > 1 min column
  for (n in 1:length(data[,1])) {
    if (is.na(data$Time.1min[n])) {
      print(data$ref[n])
    }
  }
  print("done")
}

time_change <- function(data){
  # lists row numbers for time change > 1 min
  for (n in 1:length(data[,1])) {
    if (data$Time.1min[n] == 1) {
      print(data$ref[n])
    }
  }
  print("done")
}

check_NA_BPM <- function(data){
  # checks for NAs in BPM change > 1 column
  for (n in 1:length(data[,1])) {
    if (is.na(data$BPM.Change[n])) {
      print(data$ref[n])
    }
  }
  print("done")
}

BPMchange <- function(data){
  # lists row numbers for BPM change > 1
  for (n in 1:length(data[,1])) {
    if (data$BPM.Change[n] == 1) {
      print(data$ref[n])
    }
  }
  print("done")
}

##### Results #####

# MYSE 150.937
data <- myse_0937
channel <- check_channel(data, data$Channel[2])
timechange_NA <- check_NA_timechange(data)
time_NA <- check_NA_time(data)
time_1 <- time_change(data)
BPM_NA <- check_NA_BPM(data)
BPM_1 <- BPMchange(data)

# MYSE 151.909
data <- myse_1909
channel <- check_channel(data, data$Channel[2])
timechange_NA <- check_NA_timechange(data)
time_NA <- check_NA_time(data)
time_1 <- time_change(data)
BPM_NA <- check_NA_BPM(data)
BPM_1 <- BPMchange(data)

# MYSO 150.995
data <- myso_0995
channel <- check_channel(data, data$Channel[2])
timechange_NA <- check_NA_timechange(data)
time_NA <- check_NA_time(data)
time_1 <- time_change(data)
BPM_NA <- check_NA_BPM(data)
BPM_1 <- BPMchange(data)

# MYSE 151.230
data <- myse_1230
channel <- check_channel(data, data$Channel[2])
timechange_NA <- check_NA_timechange(data)
time_NA <- check_NA_time(data)
time_1 <- time_change(data)
BPM_NA <- check_NA_BPM(data)
BPM_1 <- BPMchange(data)

# MYSE 151.312
data <- myse_1312
channel <- check_channel(data, data$Channel[2])
timechange_NA <- check_NA_timechange(data)
time_NA <- check_NA_time(data)
time_1 <- time_change(data)
BPM_NA <- check_NA_BPM(data)
BPM_1 <- BPMchange(data)

# MYSO 151.547
data <- myso_1547
channel <- check_channel(data, data$Channel[2])
timechange_NA <- check_NA_timechange(data)
time_NA <- check_NA_time(data)
time_1 <- time_change(data)
BPM_NA <- check_NA_BPM(data)
BPM_1 <- BPMchange(data)

# MYSE 151.467
data <- myse_1467
channel <- check_channel(data, data$Channel[2])
timechange_NA <- check_NA_timechange(data)
time_NA <- check_NA_time(data)
time_1 <- time_change(data)
BPM_NA <- check_NA_BPM(data)
BPM_1 <- BPMchange(data)

# MYSE 151.830
data <- myse_1830
channel <- check_channel(data, data$Channel[2])
timechange_NA <- check_NA_timechange(data)
time_NA <- check_NA_time(data)
time_1 <- time_change(data)
BPM_NA <- check_NA_BPM(data)
BPM_1 <- BPMchange(data)

# MYSO 151.786
data <- myso_1786
channel <- check_channel(data, data$Channel[2])
timechange_NA <- check_NA_timechange(data)
time_NA <- check_NA_time(data)
time_1 <- time_change(data)
BPM_NA <- check_NA_BPM(data)
BPM_1 <- BPMchange(data)

# Clean Smokies Bat Data #
# Ellen Bledsoe
# December 2015

##### Load Files #####
library(dplyr)

myse_150937 <- read.csv("data_csv/MYSE_150937.csv")
myse_151230 <- read.csv("data_csv/MYSE_151230.csv")
myse_151312 <- read.csv("data_csv/MYSE_151312.csv")
myse_151467 <- read.csv("data_csv/MYSE_151467.csv")
myse_151830 <- read.csv("data_csv/MYSE_151830.csv")
myse_151909 <- read.csv("data_csv/MYSE_151909.csv")
myso_150995 <- read.csv("data_csv/MYSO_150995.csv")
myso_151547 <- read.csv("data_csv/MYSO_151547.csv")
myso_151786 <- read.csv("data_csv/MYSO_151786.csv")

datasets <- c(myse_150937, myse_151230, myse_151312, myse_151467, myse_151830, 
              myso_150995, myso_151547, myso_151786)

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

check_NA_BPM <- function(data){
  # checks for NAs in BPM change > 1 column
  for (n in 1:length(data[,1])) {
    if (is.na(data$BPM.Change[n])) {
      print(data$ref[n])
    }
  }
  print("done")
}

channel_and_NAs <- function(data){
  channel <- check_channel(data, data$Channel[2])
  timechange_NA <- check_NA_timechange(data)
  time_NA <- check_NA_time(data)
  BPM_NA <- check_NA_BPM(data)
}

##################



##### Results #####

### Step 1

channel_and_NAs(myse_150937)
channel_and_NAs(myse_151230)
channel_and_NAs(myse_151312)
channel_and_NAs(myse_151467)
channel_and_NAs(myse_151830)
channel_and_NAs(myse_151909)
channel_and_NAs(myso_150995)
channel_and_NAs(myso_151547)
channel_and_NAs(myso_151786)

### Step 2

############### WORKING AREA ################

time_changes <- myso_151786 %>% 
                filter(Time.1min == '1')
time_change_refs <- time_changes %>% select(ref, Frequency)
write.csv(time_change_refs, "myso_151786_time_refs.csv")
BPM_changes <- myso_151786 %>% 
               filter(BPM.Change == '1')
BPM_changes_refs <- BPM_changes %>% select(ref, Frequency)
write.csv(BPM_changes_refs, "myso_151786_BPM_refs.csv")

# Clean Smokies Bat Data #
  # Ellen Bledsoe
  # December 2015

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

##### Load File #####

# figure out some way to load all
files <-  list.files("data_csv", pattern="*.csv", full.names = TRUE)

##### Results #####

# not yet working
for (f in files) {
  print(f)
  data <- read.csv(f)
  print(paste(f, "incorrect channels:"))
  channel <- check_channel(data, data$Channel[2])
  print(paste(f, "NAs in time change:"))
  timechange_NA <- check_NA_timechange(data)
  print(paste(f, "NAs in time change > 1:"))
  time_NA <- check_NA_time(data)
  print(paste(f, "rows with time change > 1:"))
  time_1 <- time_change(data)
  print(paste(f, "NAs in BPM change > 1:"))
  BPM_NA <- check_NA_BPM(data)
  print(paste(f, "rows with BPM change > 1:"))
  BPM_1 <- BPMchange(data)
}

##### Print Outputs #####

# figure this out, too
print(paste('incorrect channels:', channel))
print(paste('Rows with NAs in time change:', time_NA))
print(paste('Rows with time change > 1:', time_1))
print(paste('Rows with NAs in BPM change:', BPM_NA))
print(c('Rows with BPM change > 1:', BPMchange(data)))

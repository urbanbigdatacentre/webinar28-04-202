####################################################################################
#################### Getting download links ####################
## Since https://cycling.data.tfl.gov.uk/ contains the csv files of cycling trips, also other files for download,
## I try to 1/ save the whole page as txt 
## (rightclick->inspect->choose the network tab->rightclick->copy all as node.js fetch)
## 2/ use genXtract to extract the relevant links: detailed trip data in 2020
## 3/ read the csv files one by one and combine them as one dataframe 


library(tidyverse)
text1 <-readLines('links.txt') # the name of my txt file 

#install.packages('qdap')
library(qdap)
x<-genXtract(text1, "fetch(",",",with=TRUE) #the 
x1<-x[lapply(x,length)>0] #drop the empty lines (without 'fetch')
x2<-genXtract(x1,'https://s3-eu-west-1.amazonaws.com/cycling.data.tfl.gov.uk/usage-stats','2020.csv',with=TRUE)
x3<-x2[lapply(x2,length)>0] #drop the lines that do not end with 2020.csv

# download csv on the list x3
#library(data.table)
alltrips <- data.frame()
for (i in 1:length(x3)){
  url <- toString(x3[i])
  trips <- read.csv(url,header=T)[,1:9] # or fread
  alltrips <- rbind(alltrips, trips) #append
}
# you might want to include the 2021 files too if you intend to work on the covid resitriction file below.

####################################################################################
######### Alternatively, you might have downloaded the files in a folder #############
library(data.table)
folder <- "C:/Users/yourname/yourfolder/"  # path to folder that holds multiple .csv files

file_list15 <- list.files(path=folder) # create list of all .csv files in folder , pattern="15.csv", as I want all files in 2015
london_bikeshare15 <-
  do.call("rbind",
          lapply(file_list15,
                 function(x)
                   fread(paste(folder, x, sep=''),
                         stringsAsFactors = FALSE,select=c(1:9)))) # only 9 columns contain data
####################################################################################
## Start processing the dataset 'all trips' 
colnames(alltrips)
# check date and time format
class('Start.Date')
alltrips$dateTimeStart <- as.POSIXlt(alltrips$'Start.Date',format="%d/%m/%Y %H:%M")
alltrips$Duration <- alltrips$Duration/60 #convert to minutes

# basic cleaning
alltrips2 <- alltrips[ which(alltrips$'StartStation.Id' != alltrips$'EndStation.Id'  ), ] # exclude trips of same OD station
alltrips3 <- alltrips2[ which(alltrips2$Duration>2  ), ] # I exclude trips under 2 mins
alltrips4 <- alltrips3[ which(alltrips3$Duration<24*60), ] # over one day 

# create a date column consistent with the covid restriction file
alltrips4$date <- format(alltrips4$dateTimeStart, format="%Y-%m-%d")

# trip count and duration by day
library(dplyr)
library(magrittr)
tab_num<-alltrips4 %>% 
  group_by(date) %>%
  summarise(no_rows = length(date))
# average trip duration by day
tab_duration<-alltrips4 %>% 
  group_by(date) %>%
  summarise(xDur = mean(Duration))
trip_dur_num <- merge(tab_duration,tab_num, by="date")

# trip count by station by day
tab_numSta<-alltrips4 %>% 
  group_by(date,StartStation.Id) %>%
  summarise(no_rows = length(date))

####################################################################################
# COVID restrictions timeseries; this is interesting if we want to study lockdown restrictions and bikeshare
#https://data.london.gov.uk/dataset/covid-19-restrictions-timeseries
covidRest <- read.csv("https://data.london.gov.uk/download/covid-19-restrictions-timeseries/ae1b5b4c-3b5c-471f-b3e5-ba4fbc3eced9/restrictions_daily.csv", header=T)[,1:11]
covidRest$date <- as.POSIXlt(covidRest$date, format = "%Y-%m-%d") #convert dates 

# combine trip data and covidRest (with covid restriction dummies)
tripcnt <- merge(trip_dur_num, covidRest, by="date", all.x=TRUE)
tripcntsta <- merge(tab_numSta, covidRest, by="date", all.x=TRUE)

#pdata.frame
library(plm)
ptrips <- pdata.frame(tripcntsta, index=c("StartStation.Id", "date"))  

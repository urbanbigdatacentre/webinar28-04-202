####################################################################################
# Exploring and analysing open transport data 
# (with a focus on SCOOT data and bike share data)
# UBDC Webinar (28 April 2021) 
# Chau Man Fung
####################################################################################

## Part 1: Open transport data

# Usually authorities make both real time and historical transport data available via API (Application Programming Interface). 
# Most are in the form of json (JavaScript Object Notation). 

# Here we have three examples of traffic count (Brussels, Hull, Glasgow)
## Brussels: more information; no api key
## Hull: more straighforward; no api key
## Glasgow: count, events, parking, vms; require api key; 
## (written in a more cumbersome way but perhaps easier to see how the lists work)

# The lines of codes for setting up a database and scheduling the task for regular interval after the Glasgow example

# Two simple examples for share bike availability are included (Citibike, Nextbike)

####################################################################################

# import libararies
library(jsonlite)
library(tidyr)
library(tidyverse)
library(leaflet) #https://rstudio.github.io/leaflet/
library(htmltools)
library(htmlwidgets)

##############################################################################################
# Brussels traffic count (slightly more complicated)
# description here: https://data.mobility.brussels/traffic/api/counts/
##############################################################################################

### define a new function to help with parsing 
### https://stackoverflow.com/questions/16300344/how-to-flatten-a-list-of-lists

flattenlist <- function(x){  
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){ 
    Recall(out)
  }else{
    return(out)
  }
}

# no api key required; request real-time traffic counts
url <- "https://data.mobility.brussels/traffic/api/counts/?request=live"
req_bru <- fromJSON(paste0(url))

# the location of counters found here: 
# http://data.mobility.brussels/traffic/api/counts/?request=devices&outputFormat=csv
counter <- read.csv("http://data.mobility.brussels/traffic/api/counts/?request=devices&outputFormat=csv",header=T)

# parse 
bru <- data.frame() #an empty df
#library(tidyverse)
for (i in  1:length(req_bru$data)){ #iterate thru each counter
  skip_to_next <- FALSE 
  #some part of the lists are empty and give an error, 
  #so we use tryCatch to skip that part if error is encountered
  df <- flattenlist(req_bru$data[i])
  tryCatch(df1 <- as.data.frame(transpose(df)), error = function(e) {skip_to_next <<- TRUE})
  if(skip_to_next){next} 
  names(df1) <- gsub(".*results\\.","",names(df1)) #shorten names by removing counter name
  df1$traverse <- gsub("\\..*","",names(df)[1])#add a traverse column 
  bru <- rbind(bru, df1) #append
}

# match name of counters to real time results
bru2 <- merge(bru, counter, by.x= 'traverse',by.y='Traverse_name')
colnames(bru2) #56 columns

# map made using leaflet in html
bru2[[57]] <- paste(bru2[[1]], as.character(bru2[[2]]), sep="=") #new column for labels shown on map, character
bru1 = leaflet(bru2) %>% addTiles() %>% addMarkers(~Lon..WGS.84., ~Lat..WGS.84., label =~htmlEscape(bru2[[57]]))
saveWidget(bru1, file="bru1.html")

##############################################################################################
# Hull Scoot Loop:  (simple)
# Description here: https://opendata.hullcc.gov.uk/dataset/scoot-loop
##############################################################################################
# api key not required
url = "https://opendata.hullcc.gov.uk/dataset/30fd3969-556d-4eae-ae4c-f3f9d2cfa9e3/resource/f4ebeb7e-ee74-4fed-aa16-72b6064ae566/download/scoot_data.geojson"
req_hull  <- fromJSON(paste0(url))
hull <- req_hull$features$properties
colnames(hull) #this is pretty neat

hull1= leaflet(hull) %>% addTiles() %>% addMarkers(~longitude, ~latitude, label =~htmlEscape(name))
saveWidget(hull1, file="hull1.html")

##############################################################################################
### Glasgow City Council traffic and mobility api (count, parking, events, vms)
##############################################################################################

#**You will need to register for a subscription key.**
# Sign up here: https://gcc.portal.azure-api.net/signup

# API Key needed
# Standard keys: lower limit
# Business keys: (limit: 200 calls per minute 10000 calls per week)

#https://gcc.azure-api.net/traffic/movement?format=json
#https://gcc.azure-api.net/traffic/carparks?format=json
#https://gcc.azure-api.net/traffic/events?format=json
#https://gcc.azure-api.net/traffic/locations?format=json
#https://gcc.azure-api.net/traffic/vms?format=json
#https://gcc.azure-api.net/traffic/vmslocations?format=xml (it says xml but it's actually json)

#################################
#traffic movement
key <- 'enter your api key' # you will need to insert the api key you obtained 
url <- "https://gcc.azure-api.net/traffic/movement?format=json"

req_movement <- fromJSON(paste0(url, key))
movement<- req_movement$d2LogicalModel$payloadPublication$siteMeasurements
df <- data.frame(matrix(ncol = 2, nrow = 0),stringsAsFactors=FALSE)
op <- options(stringsAsFactors=F) 
for (i in 1:nrow(movement))
  df = rbind(df,c(movement[[3]][[i]]$basicDataValue[1,2],movement[[3]][[i]]$basicDataValue[2,3]))
colnames(df) <- c("flow", "concentration")
movement_count <- cbind(movement,df)
movement_count <- movement_count[, -c(3)]

###################################
# parking availability and location

url <- "https://gcc.azure-api.net/traffic/carparks?format=json"
req_parking <- fromJSON(paste0(url, key))
parking <- req_parking$"d2lm$d2LogicalModel"$"d2lm$payloadPublication"$"d2lm$situation"$"d2lm$situationRecord"
parking_coor <-cbind(parking,parking[[9]]$"d2lm$locationContainedInGroup"[[2]][[1]]$"d2lm$latitude",parking[[9]]$"d2lm$locationContainedInGroup"[[2]][[1]]$"d2lm$longitude")
colnames(parking_coor)
names(parking_coor)[15] <- "latitude"
names(parking_coor)[16] <- "longitude"
parking_coor <- parking_coor[, -c(8:9)] 

#################################
#traffic events
#location of these items

#events[[8]][i,1] #active
#events[[8]][i,2][1] #start time
#events[[8]][i,2][2] #end time
#events[[9]][i,1][1,1][1] #lang
#events[[9]][i,1][1,1][2] #message
#events[[10]][i,1][1,1][1] #lang
#events[[10]][i,1][1,1][2]#complete message
#events[[11]][[1]][[2]][[1]][[14]] #TPEGSimplePoint
#events[[11]][[1]][[2]][[2]][[14]] #direction
#events[[11]][[1]][[2]][[3]][[14]] #nonLinkedPoint
#events[[11]][[1]][[2]][[4]][[1]][14] #TPEGNonJunctionPoint
#events[[11]][[1]][[2]][[4]][[2]][[1]][14] #latitude
#events[[11]][[1]][[2]][[4]][[2]][[2]][14] #longitude
#events[[11]][[1]][[2]][[4]][[3]][[13]][1,2] #locallink
#events[[11]][[1]][[2]][[4]][[3]][[13]][2,2] #town
#events[[11]][[1]][[2]][[4]][[3]][[13]][3,2] #link


url <- "https://gcc.azure-api.net/traffic/events?format=json"
req_events <- fromJSON(paste0(url, key))
events <- req_events$"d2lm$d2LogicalModel"$"d2lm$payloadPublication"$"d2lm$situation"$"d2lm$situationRecord"
df2 <- data.frame(matrix(ncol = 0, nrow = 0),stringsAsFactors=FALSE)
op <- options(stringsAsFactors=F) 

for (i in 1:nrow(events))
  df2 <- append(df2,c(events[[8]][i,2][1],
                      events[[8]][i,2][2] ,
                      events[[10]][i,1][1,1][2],
                      events[[11]][[1]][[2]][[2]][[i]],
                      events[[11]][[1]][[2]][[4]][[2]][[1]][i],
                      events[[11]][[1]][[2]][[4]][[2]][[2]][i],
                      events[[11]][[1]][[2]][[4]][[3]][[i]][1,2]))
df2<-df2[!is.na(df2)]
events_text <- matrix(df2, 7)
events_text <- as.data.frame(t(events_text))
colnames(events_text) <- c("StartTime","EndTime","Message","Direction","Latitude","Longitude","Road")

#################################
#vms (variable message system)

url <- "https://gcc.azure-api.net/traffic/vms?format=json"
req_vms <- fromJSON(paste0(url, key))
vms <- req_vms$d2LogicalModel$payloadPublication$situation
vms_df <- cbind(req_vms$d2LogicalModel$payloadPublication$situation$'@id',
                req_vms$d2LogicalModel$payloadPublication$situation$situationRecord$situationRecordCreationTime,
                req_vms$d2LogicalModel$payloadPublication$situation$situationRecord$probabilityOfOccurrence,
                req_vms$d2LogicalModel$payloadPublication$situation$situationRecord$validity$validityStatus,
                req_vms$d2LogicalModel$payloadPublication$situation$situationRecord$timeLastSet,
                req_vms$d2LogicalModel$payloadPublication$situation$situationRecord$vmsLegend)
colnames(vms_df)<-c("id","CreationTime","ProbOfOccurrence","Status","timeLastSet","vmsLegend")
vms_df<-as.data.frame(vms_df)

#################################
#vms locations

url <- "https://gcc.azure-api.net/traffic/vmslocations?format=xml"
req_vmslocation <- fromJSON(paste0(url, key))
vmslocation <- req_vmslocation$d2LogicalModel$payloadPublication$predefinedLocationSet$predefinedLocation
vmslocation_df <- cbind(req_vmslocation$d2LogicalModel$payloadPublication$publicationTime,vmslocation$"@id",
                        vmslocation$predefinedLocation$tpegpointLocation$'@xsi$type',
                        vmslocation$predefinedLocation$tpegpointLocation$tpegDirection,
                        vmslocation$predefinedLocation$tpegpointLocation$tpegLocationType,
                        vmslocation$predefinedLocation$tpegpointLocation$point$'@xsi$type',
                        vmslocation$predefinedLocation$tpegpointLocation$point$'pointCoordinates'$latitude,
                        vmslocation$predefinedLocation$tpegpointLocation$point$'pointCoordinates'$longitude,
                        vmslocation$predefinedLocation$tpegpointLocation$point$name$descriptor$value$'#text')
colnames(vmslocation_df)<-c("publicationTime","id","type","tpegDirection","tpegLocationType","pointType","pointLat","pointLong","text")
vmslocation_df<-as.data.frame(vmslocation_df)

# use leaflet to make maps

class(vmslocation_df[[7]])
class(vmslocation_df[[8]])
# convert pointlong and pointlat columns into numbers (coordinates)
vmslocation_df[[7]] <- sapply(vmslocation_df[[7]], as.numeric)
vmslocation_df[[8]] <- sapply(vmslocation_df[[8]], as.numeric)

# combine vms_df and vmslocation_df
# the column of vms id in vms_df has extra characters; have to remove to merge with vmslocation_df
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
vms_df[[1]] <- substrRight(vms_df[[1]], 6)
vms <- merge(vmslocation_df, vms_df, by = 'id')
vms[[15]] <- substr(vms[[14]], 3,nchar(vms[[14]])-1) #extract the content of the message

############################################
## this creates a map in html for vms only, with labels showing the messages

#library(tidyr)
#library(htmltools)
vms1 = leaflet(vms) %>% addTiles() %>% addMarkers(~pointLong, ~pointLat, label =~htmlEscape(vms[[15]]))
#library(htmlwidgets)
saveWidget(vms1, file="vms1.html")

############################################
## this creates a map in html for parking only, with labels showing the availability
parking_coor[[13]] <- sapply(parking_coor[[13]], as.numeric)
parking_coor[[14]] <- sapply(parking_coor[[14]], as.numeric)

parking1 = leaflet(parking_coor) %>% addTiles() %>% addMarkers(~longitude, ~latitude, label =~htmlEscape(parking_coor[[10]]))
saveWidget(parking1, file="parking1.html")

########################################################################################
## this creates a map in html for SCOOT locations only, with labels showing the trafic count

# the location of SCOOT is not provided in the real-time data we request
# but this is available from the historical dataset requested (df); the settings such as size, site, can be modified
df <- fromJSON("http://gcc.azure-api.net/traffic/v1/movement/history?%spage&size=100&format&site&start&end&duration", flatten = TRUE) 
df0 <- fromJSON("http://gcc.azure-api.net/traffic/v1/movement/history?%spage&size=100&format&site&start&end&duration") 
#set size =100 to cover all sites; there are around 6x sites
#this one is neat but not in real-time; if you set a large size and also set duration, you can obtain historical data as early as May 2017
colnames(df) # 19
colnames(df0) # 9 with 'site'
# it is better to set the flatten option to TRUE in this case because the list 'site' can be easily flattened
df2 = df[!duplicated(df$site.siteId),] # drop duplicated siteId
# drop some columns
df3 = df2[,c('site.siteId','site.from.lat','site.from.long','site.from.description','site.to.lat','site.to.long','site.to.description')]
# drop row with zero long or lat
row_sub = apply(df3, 1, function(row) all(row !=0 ))
df4 <- df3[row_sub,]
#merge with count
count2 <- merge(df4, movement_count, by.x= 'site.siteId',by.y='measurementSiteReference')

count2[['site.from.long']] <- sapply(count2[['site.from.long']], as.numeric)
count2[['site.from.lat']] <- sapply(count2[['site.from.lat']], as.numeric)
count1 = leaflet(count2) %>% addTiles() %>% addMarkers(~site.from.long, ~site.from.lat, label =~htmlEscape(flow))
saveWidget(count1, file="count1.html")

#putting all three together
icon.parking <- makeAwesomeIcon(icon= 'flag',library='fa', markerColor = 'orange', iconColor = 'black')
icon.count <- makeAwesomeIcon(icon = 'car',library='fa', markerColor = 'blue', iconColor = 'black')
icon.vms <- makeAwesomeIcon(icon = 'comments',library='fa', markerColor = 'green',  iconColor = 'black')

m=leaflet() %>% 
  addTiles() %>% 
  addAwesomeMarkers(data=parking_coor,~longitude, ~latitude, label =~htmlEscape(parking_coor[[10]]), group = 'parking',icon=icon.parking,labelOptions = labelOptions(noHide = F)) %>%
  addAwesomeMarkers(data=count2,~site.from.long, ~site.from.lat, label =~htmlEscape(flow),group='count',icon=icon.count,labelOptions = labelOptions(noHide = F)) %>%
  addAwesomeMarkers(data=vms,~pointLong, ~pointLat, label =~htmlEscape(vms[[15]]),group='vms',icon=icon.vms,labelOptions = labelOptions(noHide = T)) %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("count", "parking","vms"),
    options = layersControlOptions(collapsed = FALSE)
  )
saveWidget(m, file="combined.html")


#For better management and a consistent historical dataset
#We can set up requests at specified intervals and save data in a database 

####################################################################################
#connect to postgresql (you will need to setup postresql first) 
#using the Glasgow data collected above
#https://www.postgresql.org/
####################################################################################

library("RPostgreSQL")
m <- dbDriver("PostgreSQL")
con <- dbConnect(m, user="user", password="password",                   
                 dbname="postgres", host="127.0.0.1",port="0000")
#write data to db

dbWriteTable(con, name='parking_space', parking_coor,append=TRUE)  
dbWriteTable(con, name='movement_count',movement_count,append=TRUE) 
dbWriteTable(con, name='vms_location',vmslocation_df,append=TRUE) 
dbWriteTable(con, name='events_text', events_text,append=TRUE) 
dbWriteTable(con, name='vms',vms_df,append=TRUE) 

# **Scheduling the tasks**<br>
#   We could set this task using schedulerR addin. <br>
#   link: https://cran.r-project.org/web/packages/taskscheduleR/vignettes/taskscheduleR.html for Windows <br>
#   There are many other ways for Mac.<br>

####################################################################################
# scheduling the task (windows only)
####################################################################################

# the scheduler addin in R studio is ok. (easier)
# https://cran.r-project.org/web/packages/taskscheduleR/vignettes/taskscheduleR.html

# if you prefer a script instead of the addin
library(taskscheduleR)
myscript <- system.file("exdata","nameOfScript.r", package = "taskscheduleR")

## run script every 5 mins
taskscheduler_create("exdata",taskname = "nameYourTask", rscript = myscript, 
                     schedule = "MINUTE", starttime = "16:55", modifier = 5)


####################################################################################
###  bikeshare data (gbfs, data dump)
####################################################################################


# Some available bikeshare data:
#   Bluebikes: https://www.bluebikes.com/system-data
#   Citibike: https://www.citibikenyc.com/system-data 
#   Capital bikshare: https://www.capitalbikeshare.com/system-data 
#   Just Eat Cycles: https://edinburghcyclehire.com/open-data/historical 
#   Divvy Bikes: https://www.divvybikes.com/system-data 
#   Healthy Ride (Nextbike): https://healthyridepgh.com/data/ 
#   Bike Share Toronto: https://open.toronto.ca/dataset/bike-share-toronto-ridership-data/
#   Nice Ride: https://www.niceridemn.com/system-data 
#   Bike Town: https://www.biketownpdx.com/system-data 
#   CoGo Bike Share: https://www.cogobikeshare.com/system-data 
#   mobi: https://www.mobibikes.ca/en/system-data 
#   Coast: http://coastbikeshare.com/data/ 
#   Indego: https://www.rideindego.com/about/data/ 

# There are broadly two types of bikeshare data commonly available.
#   One is historical trip data (usually in json or csv format, data dump); 
#   frequency of updating depends on the data owner
#   Some examples:
#   https://bikeshare.metro.net/about/data/ 
#   https://edinburghcyclehire.com/open-data/historical 

# details of GBFS: https://github.com/NABSA/gbfs/blob/master/gbfs.md#free_bike_statusjson

####################################################################################
# citibike
####################################################################################
citibike <- fromJSON("http://citibikenyc.com/stations/json")
stations_citibike <- citibike$stationBeanList
colnames(stations_citibike)

####################################################################################
# Next Bike
####################################################################################

# https://api.nextbike.net/maps/gbfs/v1/nextbike_gg/gbfs.json This is the link of api in the email. 
# For the other bike share companies, just click on "real time/ live GBFS feed" for all the links with available info.

# What I found for next bike:
#{"name":"system_information","url":"https://gbfs.nextbike.net/maps/gbfs/v1/nextbike_gg/en/system_information.json"},
#{"name":"station_information","url":"https://gbfs.nextbike.net/maps/gbfs/v1/nextbike_gg/en/station_information.json"}
#{"name":"station_status","url":"https://gbfs.nextbike.net/maps/gbfs/v1/nextbike_gg/en/station_status.json"}
#{"name":"free_bike_status","url":"https://gbfs.nextbike.net/maps/gbfs/v1/nextbike_gg/en/free_bike_status.json"}
#{"name":"system_hours","url":"https://gbfs.nextbike.net/maps/gbfs/v1/nextbike_gg/en/system_hours.json"}
#{"name":"system_regions","url":"https://gbfs.nextbike.net/maps/gbfs/v1/nextbike_gg/en/system_regions.json"}

# station status shows the number of bikes and docks available at the time shown
# run this line first, and click the list open to see the path of the table you want 
nextbike_stationstatus <- fromJSON("https://gbfs.nextbike.net/maps/gbfs/v1/nextbike_gg/en/station_status.json")
# it's nextbike_stationstatus -> data -> station, then you see the table you are looking for
stations_nextbike <- nextbike_stationstatus$data$stations
colnames(stations_nextbike)


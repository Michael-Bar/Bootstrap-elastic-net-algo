##combine metadata with awhere data

## Fresh package install
#devtools::install_github("aWhereAPI/aWhere-R-Library", force = T)


library(maptools)
library(ggplot2)
library(rgdal)
library(maptools)
library(dismo)
library(leaflet)
library(sp)
library(XML)
library(readr)
library(aWhereAPI)
library(lubridate)
library(data.table)
library(curl)


#func
getNormWeather <- function(dat, row){
  t <- try(get_fields(dat[row, "nom"]))
  if(inherits(t, "try-error")) {
    create_field(dat[row,"nom"], dat[row,"Latitude"], dat[row,"Longitude"], dat[row, "nom"])
  }
  val = agronomic_norms_fields(dat[row, "nom"], norm_start, norm_end, norm_start_year, norm_end_year)
  return(val)
}





getdayWeather <- function(dat2, row){
  t <- try(get_fields(dat2[row, "nom"]))
  if(inherits(t, "try-error")) {
    create_field(dat2[row,"nom"], dat2[row,"Latitude"], dat2[row,"Longitude"], dat2[row, "nom"])
  }
  val = odaily_observed_fields(field_id = dat2[row,"nom"], startdate, stopdate)
  return(val)
}

######

#load in locally modified functions to avoid certain issues with daily data
#D:/OneAcre/Google Drive/One Acre Fund/optimized_agronomy/weather

source("D:/OneAcre/Google Drive/One Acre Fund/optimized_agronomy/weather/weather_norms_latlng.R")
source("D:/OneAcre/Google Drive/One Acre Fund/optimized_agronomy/weather/daily_observed_latlng.R")
source("D:/OneAcre/Google Drive/One Acre Fund/optimized_agronomy/weather/agronomic_norms_latlng.R")

source("D:/OneAcre/Google Drive/One Acre Fund/optimized_agronomy/weather/daily-observed.R")

#orig
source("D:/OneAcre/Google Drive/One Acre Fund/optimized_agronomy/weather/awhr_daily.R")

#orig recenter
source("D:/OneAcre/Google Drive/One Acre Fund/optimized_agronomy/weather/awhr_weather_daily.R")

#this one for daily
source("D:/OneAcre/Google Drive/One Acre Fund/optimized_agronomy/weather/daily_observed_fields.R")

#daily_observed_fields.R

#daily-observed.R 
#test file paths
#x <- dat[1,]
#write.csv(x, file="D:/OneAcre/Google Drive/One Acre Fund/optimized_agronomy/weather/test.csv")
start.time <- proc.time()

dat <- read_csv("C:/Users/Michael/OneDrive/OAF/MB/Projects/Roster_data/Kenya/2016_site_Client_plant_gps_full.csv")
colnames(dat)

# First check GPS points are sensible -------------------------------------

#remove any rows with NA Latitude
dat <- dat[complete.cases(dat$Latitude),]

ss <- SpatialPointsDataFrame(coords = dat[, c("Longitude", "Latitude")], data=dat)



#map out points to check logic
ke <- try(geocode("Kenya"))
#pal <- colorNumeric(c("navy", "green"), domain=unique(ss$oaf))
map <- leaflet() %>% addTiles() %>%
  setView(lng=ke$longitude, lat=ke$latitude, zoom=1) %>%
  addCircleMarkers(lng=ss$Longitude, lat=ss$Latitude,
                   clusterOptions = markerClusterOptions(disableClusteringAtZoom=12, spiderfyOnMaxZoom=FALSE))

map
#likewie min max Lat and Lon
summary(dat$Longitude)
summary(dat$Latitude)

#manual check

# Now start aWhere data API --------------------------------------


##define start and end dates for data pull
##lets first look at historical data, i.e. exclude 2016 (which is the year we're looking at for repay)
norm_start <- "01-01"
norm_end <- "12-31"
norm_start_year <- "2005"
norm_end_year <- "2015"

##use passkey
get_token("XSLgzFVPP9rIuWoru5LRdQRgtnuAzx70", "nFrmmwNSsA5q3uHk")


##smaller dataframe to test
subdat <- dat

startdate <- "2016-01-01"
stopdate <- "2016-12-31"
# 
# #orig
# #edited rbind call
# source("D:/OneAcre/Google Drive/One Acre Fund/optimized_agronomy/weather/awhr_daily.R")
# test <- NA
# test <- awdaily_observed_fields(field_id = subdat[20,"nom"], startdate, stopdate)
# dim(test)
# 
# #orig recenter
# test <- atwodaily_observed_fields(field_id = subdat[20,"nom"], startdate, stopdate)
# dim(test)
# 
# #OAF edited - gives 268
# test <- odaily_observed_fields(field_id = subdat[20,"nom"], startdate, stopdate)
# dim(test)
# 
# #awhere tech support
# 
# 



###end tech support





subda <- dat











#daily_observed_fields(, day_start, day_end)

start.time <- proc.time()
dim(subda)
res <- NULL
ix <- 1
for(i in ix:dim(subda)[1]){
  ix <- i
  pt <-proc.time()
  print(i/dim(subda)[1]*100)

  out <- getNormWeather(subda, i)
  res <- rbind(res, out)
  print(dim(res))
  print(proc.time()-pt)
}

dim(res)

dim(subda)
colnames(res)


#write.csv(res, file="full_awhere_05-15.csv", row.names = FALSE)

##test out other functions
startdate <- "2016-01-01"
stopdate <- "2016-12-31"

# delete_field(subda[1,"nom"] )
# create_field(subda[1,"nom"], subda[1,"Latitude"], subda[1,"Longitude"], subda[1, "nom"])
# getdayWeather(subda, 1)
# dim(getdayWeather(subda, 1))



re <- NULL


ix <- 1

#if it stalls (bad internet etc), start from the loop (here) again and it *should* be fine

for(i in ix:dim(subda)[1]){
  ix <- i
  print(i)
  print(paste(round(i/dim(subda)[1]*100,4),"%"))
  pt <-proc.time()
  ou <- getdayWeather(subda, i)
  re <- rbind(re, ou) 
  print(paste("DIMS",dim(re)))
  print(proc.time()-pt)
  }


dim(re)
dim(subda)


#write.csv(re, file="full_awhere_16.csv", row.names = FALSE)

proc.time() - start.time


awhr <- res
awhr16 <- re

sitedat <- dat


awhr$semi.name <- "A"
awhr16$semi.name <- "B"

x <- seq(1:dim(sitedat)[1])
x<- (x*366)-365
for(i in 1:length(x)   ){
  
  loc <- x[i]
  loc2 <- loc+366
  nam <- sitedat$semi.name[i]
  awhr[loc:loc2,]$semi.name <- nam
  awhr16[loc:loc2,]$semi.name <- nam }

write.csv(awhr, "C:/Users/Michael/OneDrive/OAF/MB/Projects/Roster_data/Kenya/aWhere/full_awhere_05-15.csv")
write.csv(awhr16,"C:/Users/Michael/OneDrive/OAF/MB/Projects/Roster_data/Kenya/aWhere/full_awhere_16.csv")




stop()


#clear all fields
#delete fields in account
while( dim(get_fields())[1] >= 1 ) {
  lapply(get_fields()$fieldId, delete_field)
}

for(i in 1:5){
  todel <- get_fields()
  print(todel$fieldId)
  lapply(todel$fieldId,delete_field)
}






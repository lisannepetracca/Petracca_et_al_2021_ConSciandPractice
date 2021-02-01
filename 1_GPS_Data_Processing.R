# R code associated with 

# Petracca, L.S., Frair, J.L., Bastille-Rousseau, G., Macdonald, D.W., and A.J. Loveridge. 
# 2021. Harassment-induced changes in lion space use as a conflict mitigation tool. 
# Conservation Science and Practice.

# This code represents a processing of GPS data for use in the paper

# The GPS data are protected and cannot be shared, but the steps below can be used for any .csv with coordinate information

library(adehabitatLT)
library(data.table)
library(rgdal)
library(maptools)
library(rio)
library(ggplot2)
library(here)

#### ---- CREATING CSV OF POINTS SEPARATED BY MINIMUM ONE HOUR ---- ####

here()

#create vector of Excel files to read
files.to.read = list.files(pattern="csv")

#create list of csvs
list.filenames<-list.files(pattern=".csv$")
list.filenames

#creates list of 18 .csvs, in which each element is individual csv
l <- lapply(list.filenames, fread, sep=",")

#binds these csvs together into single file
dt <- rbindlist(l, fill=TRUE)

#creates data frame containing only four variables of interest
alldata <- as.data.frame(subset(dt, select=c("Name", "LocalTime", "lat", "long")))

#checks to ensure all NAs are gone
sum(is.na(alldata$Name))
head(alldata)
tail(alldata)
unique(alldata$Name)

#convert time column to POSIXlt object and then POSIXct object
alldata$time <- as.character(strptime(alldata$LocalTime,"%m/%d/%y %H:%M:%S"))
alldata$time <- as.POSIXct(alldata$time, tz="Africa/Harare")

#remove duplicates
alldata$checkdup <- paste(alldata$Name, alldata$LocalTime)
newdata <- alldata[!duplicated(alldata$checkdup), ]
newdata$time <- as.POSIXct(newdata$time, tz="Africa/Harare")

#now that we have removed exact duplicates, lets round to nearest hour and remove duplicates
newdata$roundtime <- round(as.POSIXct(newdata$time, format="%H", tz="Africa/Harare"), units="hours")
newdata$test <- paste(newdata$Name, newdata$roundtime)
newdata <- newdata[!duplicated(newdata$test), ]
newdata$time <- as.POSIXct(newdata$roundtime, tz="Africa/Harare")

#extract WGS coordinate columns from alldata
WGSCoords <- data.frame(X = newdata$long, Y = newdata$lat)
names(WGSCoords) <- c("X","Y")
head(WGSCoords)

#convert it from data frame to sp object
coordinates(WGSCoords) <- ~ X + Y # longitude first

#add a coordinate reference system (in this case WGS84)
proj4string(WGSCoords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

#project using spTransform
UTMCoords <- as.data.frame(spTransform(WGSCoords, CRS("+proj=utm +zone=35 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))

#conversion to class ltraj
#these data are now all those that are separated by at least one hour!
finaldata <- as.ltraj(xy = UTMCoords[,c("X","Y")], date = newdata$time, id=newdata$Name, burst=newdata$Name)
sum(summary(finaldata)$nb.reloc)

finaldata <-ld(finaldata)
write.csv(finaldata, "finaldata.csv")

#### ---- NOW CREATING CSV OF POINTS IN WHICH POINTS SEPARATED BY EXACTLY 2 HRS ----####

#now we subsample such that there are points every hour
#we are doing this because resample requires that the trajectories are regular

#this creates 729701 relocations (obvs lots of NAs)
refda <- strptime("2004-01-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="Africa/Harare")
datastep_hourly <- setNA(finaldata, refda, 1, units=c("hour"))
sum(summary(datastep_hourly)$nb.reloc)

#get number of bursts
burst(datastep_hourly)

x <- rep(1, times=18) 

subsample <- function (ltraj, dt, nlo, units = c("sec", "min", "hour",
                                                 "day"), ...)
{
  if (!inherits(ltraj, "ltraj"))
    stop("ltraj should be of class \"ltraj\"")
  if ((!is.regular(ltraj)) & (attr(ltraj, "typeII")))
    stop("ltraj should be of type I or type II regular")
  p4s <- adehabitatLT:::.checkp4(ltraj)
  if (length(nlo) == 1)
    nlo <- rep(nlo, length(ltraj))
  units <- match.arg(units)
  dt <- adehabitatLT:::.convtime(dt, units)
  dtb <- ltraj[[1]]$dt[1]
  if (dt%%dtb != 0)
    stop("dt is not a multiple of the previous time lag")
  la <- dt/dtb
  res <- lapply(1:length(ltraj), function(i) {
    x <- ltraj[[i]]
    infol <- attr(x, "infolocs")
    vec <- rep(1:la, length = nrow(x))
    x <- x[vec == nlo[i], ]
    if (!is.null(infol)) {
      infol <- infol[vec == nlo[i], , drop = FALSE]
      attr(x, "infolocs") <- infol
    }
    return(x)
  })
  class(res) <- c("ltraj", "list")
  attr(res, "typeII") <- attr(ltraj, "typeII")
  attr(res, "regular") <- is.regular(res)
  attr(res, "proj4string") <- p4s
  res <- rec(res, ...)
  return(res)
}

#and then use this hourly layer to subsample such that points are separated by 2 hrs
datastep_2hr <- subsample(datastep_hourly, dt = 7200, nlo=x, units=c("sec"))
summary(datastep_2hr)

#getting rid of NAs
#issue is that, while all points are separated by min 2 hrs, we need to re-create ltraj 
#as some points separated by 12 hrs

#leaves us with 119805 points from 234165 (51.2%)
datastep_2hr <- na.omit(datastep_2hr)
sum(summary(datastep_2hr)$nb.reloc)
datastep_2hr[[1]]

#now we need to remake this such that max time separating is 2 hr
#function "foo2" will return TRUE when time lag btw 2 relocs is >2 hours

foo2 <- function(dt) 
{
  return(dt> (2*3600))
}
#then, cutltraj will cut any burst relocs w vaue of dt such that foo(dt) is true
#results in deletion of 8583 points, as there were likely lone rangers that were not part of 3+ points
#from 234165 to 111222 points, yikes (47.5%)
datastep_2hr_final <- cutltraj(datastep_2hr, "foo2(dt)", nextr = TRUE)
sum(summary(datastep_2hr_final)$nb.reloc)

#make dataframe of these points and then export as .csv
datastep_2hr_final <-ld(datastep_2hr_final)
write.csv(datastep_2hr_final, "finaldata_2hronly.csv")

#final stats are that using all data sep by min 1 hr is 234165 points (finaldata.csv)
#using all data separated by 2 hours only is 111222 points (47.5%) (finaldata_2hronly.csv)

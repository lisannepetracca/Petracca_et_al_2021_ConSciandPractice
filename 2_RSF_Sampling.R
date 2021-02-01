# R code associated with 

# Petracca, L.S., Frair, J.L., Bastille-Rousseau, G., Macdonald, D.W., and A.J. Loveridge. 
# 2021. Harassment-induced changes in lion space use as a conflict mitigation tool. 
# Conservation Science and Practice.

# This code represents the sampling of covariates for use in RSF

# input .csvs (finaldata.csv and finaldata_2hronly.csv) come from "1_GPS_Data_Processing.R"

library(here)
library(sf)
library(raster)
library(adehabitatHR)
library(sp)
library(dplyr)
library(spdep)
library(rgdal)
library(maptools)

here()

#read in data
#reading in the data separated by exactly 2 hrs was in response to reviewer comment to investigate whether
#variable fix rate affected RSF coefficients (it did not)
data <- read.csv("finaldata.csv", header=T)
data2 <- read.csv("finaldata_2hronly.csv", header=T)

#lets work w all points first, match to data2 values via lookup table
#extracting date metrics
data$time <- as.POSIXct(strptime(data$date,"%Y-%m-%d %H:%M:%S"), tz="Africa/Harare")
data$jday<-as.numeric(format(data$time, "%j"))
data$year<-as.numeric(format(data$time, "%Y"))
data$hour<-as.numeric(format(data$time, "%H"))

data$jday<-ifelse(data$jday==366, 365, data$jday)

#create cols for JDay and Year using strptime
#replace j with y to get year; Y for 4-digit year
#defining before/after and wet/dry

#set before/after and day/night
data$after <- ifelse((data$year>2012) | (data$year==2012 & data$jday>=122),"after", "before") 
data$night <- ifelse((data$hour>=18) | (data$hour<=7),"night", "day")
data$wet <- 0

#now set wet season
data$wet[(data$year==2005 & data$jday>=305) | (data$year==2006 & data$jday<=120)] <- 1
data$wet[(data$year==2006 & data$jday>=305) | (data$year==2007 & data$jday<=120)] <- 1
data$wet[(data$year==2007 & data$jday>=305) | (data$year==2008 & data$jday<=121)] <- 1
data$wet[(data$year==2008 & data$jday>=306) | (data$year==2009 & data$jday<=120)] <- 1
data$wet[(data$year==2009 & data$jday>=305) | (data$year==2010 & data$jday<=120)] <- 1
data$wet[(data$year==2010 & data$jday>=305) | (data$year==2011 & data$jday<=120)] <- 1
data$wet[(data$year==2011 & data$jday>=305) | (data$year==2012 & data$jday<=121)] <- 1
data$wet[(data$year==2012 & data$jday>=306) | (data$year==2013 & data$jday<=120)] <- 1
data$wet[(data$year==2013 & data$jday>=305) | (data$year==2014 & data$jday<=120)] <- 1
data$wet[(data$year==2014 & data$jday>=305) | (data$year==2015 & data$jday<=120)] <- 1
data$wet[(data$year==2015 & data$jday>=305) | (data$year==2016 & data$jday<=121)] <- 1

data$season[(data$year==2005 & data$jday>=305) | (data$year==2006 & data$jday<=120)] <- "Wet 2006"
data$season[(data$year==2006 & data$jday>=305) | (data$year==2007 & data$jday<=120)] <- "Wet 2007"
data$season[(data$year==2007 & data$jday>=305) | (data$year==2008 & data$jday<=121)] <- "Wet 2008"
data$season[(data$year==2008 & data$jday>=306) | (data$year==2009 & data$jday<=120)] <- "Wet 2009"
data$season[(data$year==2009 & data$jday>=305) | (data$year==2010 & data$jday<=120)] <- "Wet 2010"
data$season[(data$year==2010 & data$jday>=305) | (data$year==2011 & data$jday<=120)] <- "Wet 2011"
data$season[(data$year==2011 & data$jday>=305) | (data$year==2012 & data$jday<=121)] <- "Wet 2012"
data$season[(data$year==2012 & data$jday>=306) | (data$year==2013 & data$jday<=120)] <- "Wet 2013"
data$season[(data$year==2013 & data$jday>=305) | (data$year==2014 & data$jday<=120)] <- "Wet 2014"
data$season[(data$year==2014 & data$jday>=305) | (data$year==2015 & data$jday<=120)] <- "Wet 2015"
data$season[(data$year==2015 & data$jday>=305) | (data$year==2016 & data$jday<=121)] <- "Wet 2016"

data$season[(data$year==2006 & data$jday>=121) & (data$year==2006 & data$jday<=304)] <- "Dry 2006"
data$season[(data$year==2007 & data$jday>=121) & (data$year==2007 & data$jday<=304)] <- "Dry 2007"
data$season[(data$year==2008 & data$jday>=122) & (data$year==2008 & data$jday<=305)] <- "Dry 2008"
data$season[(data$year==2009 & data$jday>=121) & (data$year==2009 & data$jday<=304)] <- "Dry 2009"
data$season[(data$year==2010 & data$jday>=121) & (data$year==2010 & data$jday<=304)] <- "Dry 2010"
data$season[(data$year==2011 & data$jday>=121) & (data$year==2011 & data$jday<=304)] <- "Dry 2011"
data$season[(data$year==2012 & data$jday>=122) & (data$year==2012 & data$jday<=305)] <- "Dry 2012"
data$season[(data$year==2013 & data$jday>=121) & (data$year==2013 & data$jday<=304)] <- "Dry 2013"
data$season[(data$year==2014 & data$jday>=121) & (data$year==2014 & data$jday<=304)] <- "Dry 2014"
data$season[(data$year==2015 & data$jday>=121) & (data$year==2015 & data$jday<=304)] <- "Dry 2015"
data$season[(data$year==2016 & data$jday>=122) & (data$year==2016 & data$jday<=305)] <- "Dry 2016"

data$seasonID <- paste(data$burst, data$season)
x <- data.frame(table(data$seasonID))
sum(x$Freq)
unique(data$seasonID)

#get rid of seasons with <200 points
data <- filter(data, seasonID != "Backpans Dry 2013" & 
                 seasonID != "Cecil Dry 2013" &
                 seasonID != "Daniel Dry 2008" &
                 seasonID != "Daniel Dry 2009" &
                 seasonID != "Daniel Dry 2010" &
                 seasonID != "Daniel Wet 2010" &
                 seasonID != "Frisky Dry 2013" &
                 seasonID != "Jericho Wet 2012" &
                 seasonID != "Kakori Dry 2014" &
                 seasonID != "OldGuvalala Dry 2014" &
                 seasonID != "SpiceJunior Dry 2012" &
                 seasonID != "Vundhla Wet 2011")

write.csv(data, "finaldata_min200pts.csv")


######------ CREATING MCP OF ALL POINTS FOR AVAILABILITY ------######

data <- read.csv("finaldata_min200pts.csv", header=T)
data <- data[ -c(1:2) ]

#create mcp
coords = data[,c("x","y")]
dim(coords)
xy <- SpatialPoints(coords, proj4string=CRS( "+proj=utm +zone=35 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), bbox = NULL)
mcp <- mcp(xy, percent=100, unin = c("m"),unout = c("km2"))
shapefile(mcp, filename='mcp.shp')

######------ CREATING KERNEL RANGES AND CORE ------######

#create spatial points data frame
datasp <- SpatialPointsDataFrame(coords=xy, data=as.data.frame(data$seasonID))
head(datasp)

#create utilization distribution
seasonalHR <- kernelUD(datasp[,1], h="href", grid=1000, same4all=FALSE)
head(seasonalHR)

#get 95th and 50th percent kernel HRs
seasonalHR_95kernel <- getverticeshr(seasonalHR,95)
seasonalHR_50kernel <- getverticeshr(seasonalHR,50)

#read to shapefile
proj4string <- CRS("+proj=utm +zone=35 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
seasonalHR_95kernel@proj4string <- proj4string
seasonalHR_50kernel@proj4string <- proj4string
shapefile(seasonalHR_95kernel, "seasonalHR_95kernel.shp")
shapefile(seasonalHR_50kernel, "seasonalHR_50kernel.shp")

######------ CREATING RANDOM POINTS WITHIN MCP ------######

coordsrandom <- spsample(mcp,n=233014,"random")
shapefile(coordsrandom, filename='randompts.shp')

######------ COMBINE USED AND RANDOM POINTS ------######

#assign used to used points
randompts <- as.data.frame(coordsrandom)
head(randompts)
data <- data[c(1:3,11,14:22)]
data$used <- 1

#assign random to random pts & duplicate columns from used
cols <- c(3,11,14:22)
randompts[,3:13] <- data[,cols]
head(randompts)
randompts$used <- 0

RSFdata <- rbind(data, randompts)
#final data has 233014 used & 233014 available, for 466028 final pts


######------ EXTRACT COVARIATES ------######

#get distance to waterhole
dist_waterhole <- raster("DistWaterhole.img")

#convert to spatial points
coords = RSFdata[,c("x","y")]
coords <- SpatialPoints(coords, proj4string=CRS( "+proj=utm +zone=35 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), bbox = NULL)
head(coords)

#get distance to waterhole for the points
distwaterhole <- extract(dist_waterhole, coords)
RSFdata$distwater <- distwaterhole

###### ------ DISTANCE TO VILLAGE ------ ######

dist_village <- raster("DistVillage.img")
plot(dist_village)

#extract distance to village for points
distvill <- extract(dist_village, coords)
RSFdata$distvill <- distvill

######------ EXTRACT NDVI ------######

### Generate daily spline raster - maybe only for pixel of interest - for multi-anual average and since 2008
#builds spline based on entire time series (build only for GPS points bc will take too much time), and you can extract daily values for that trend (more or less)

#x and y coordinate only, maintains order
dataNDVI<-SpatialPoints(RSFdata[,1:2], proj4string =CRS("+proj=utm +zone=35 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
dataNDVI_proj <- spTransform(dataNDVI, CRS("+proj=utm +zone=35 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

#convert to spdf
dataNDVI <- SpatialPointsDataFrame(coords = dataNDVI_proj, data = RSFdata[,3:16],
                                   proj4string = CRS("+proj=utm +zone=35 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

#create stack of raster images
files<-c(paste(2004, dir("NDVI_smooth/2004"), sep="/"),paste(2005, dir("NDVI_smooth/2005"), sep="/"),
         paste(2006, dir("NDVI_smooth/2006"), sep="/"),paste(2007, dir("NDVI_smooth/2007"), sep="/"),paste(2008, dir("NDVI_smooth/2008"), sep="/"),
         paste(2009, dir("NDVI_smooth/2009"), sep="/"), paste(2010, dir("NDVI_smooth/2010"), sep="/"),paste(2011, dir("NDVI_smooth/2011"), sep="/"),paste(2012, dir("NDVI_smooth/2012"), sep="/"),
         paste(2013, dir("NDVI_smooth/2013"), sep="/"),paste(2014, dir("NDVI_smooth/2014"), sep="/"),paste(2015, dir("NDVI_smooth/2015"), sep="/"), paste(2016, dir("NDVI_smooth/2016"), sep="/") )

setwd("NDVI_smooth")				
ndvi<-stack(files)
NAvalue(ndvi)<--3000

#this is converting stack to matrix
ndvi_curr_mat<-values(ndvi)

#Subset to only keep relevant rows
ndvi_pix<-extract(ndvi[[1]], dataNDVI, cellnumbers=T)
ndvi_pix<-sort(unique(ndvi_pix[,1]))

#keeping only rows for which you have GPS points
ndvi_curr_mat<-ndvi_curr_mat[ndvi_pix,]

## Perform spline 
#I need to start in 2004
#353 is last day of year for which you have NDVI value
tt5<-function(x) {
  if (sum(is.na(x))==200) {
    out<-rep(NA, 3161) }
  
  if (sum(is.na(x))!=23) {
    #tt length should match #cols in matrix, or # composites in stack
    tt<-c(seq(1,353,16),seq(1,353,16)+365, seq(1,353,16)+365*2, seq(1,353,16)+365*3, seq(1,353,16)+365*4, seq(1,353,16)+365*5, seq(1,353,16)+365*6, seq(1,353,16)+365*7, 
          seq(1,353,16)+365*8, seq(1,353,16)+365*9, seq(1,353,16)+365*10, seq(1,353,16)+365*11, 
          seq(1,241,16)+365*12 )
    #tt is time in spline, x is actual NDVI vals; saying you want 3161 values of NDVI; you only want NDVI output, so $y
    out<-spline(tt, x, n=4621)$y
    
  }
  return(out)
}


#this is where you apply function to pixels
#applying to every row in matrix (those pixels w GPS points)
spline_ndvi_curr<-data.frame(t(apply(ndvi_curr_mat, 1, tt5)))
spline_ndvi_curr$cells<-ndvi_pix

#this will tell you which row in spline matrix to extract
RSFdata$ndvi_cells<-extract(ndvi[[1]], dataNDVI, cellnumbers=T)[,1]

#replace values of 366 with 365
RSFdata$jday<-ifelse(RSFdata$jday==366, 365, RSFdata$jday)

#tells you which column from spline matrix you want to extract
#i will need to start this from 2004
RSFdata$Column<-ifelse(RSFdata$year==2004, RSFdata$jday,
                       ifelse(RSFdata$year==2005, RSFdata$jday+365*1, 
                              ifelse(RSFdata$year==2006, RSFdata$jday+365*2, 
                                     ifelse(RSFdata$year==2007, RSFdata$jday+365*3, 
                                            ifelse(RSFdata$year==2008, RSFdata$jday+365*4, 
                                                   ifelse(RSFdata$year==2009, RSFdata$jday+365*5, 
                                                          ifelse(RSFdata$year==2010, RSFdata$jday+365*6, 
                                                                 ifelse(RSFdata$year==2011, RSFdata$jday+365*7, 
                                                                        ifelse(RSFdata$year==2012, RSFdata$jday+365*8,
                                                                               ifelse(RSFdata$year==2013, RSFdata$jday+365*9,
                                                                                      ifelse(RSFdata$year==2014, RSFdata$jday+365*10, 
                                                                                             ifelse(RSFdata$year==2015, RSFdata$jday+365*11, RSFdata$jday+365*12))))))))))))


write.csv(data, "GPS_cluster_all.csv", row.names=F)

data<-read.csv("GPS_cluster_all.csv")
head(data)

#extracting values of NDVI
id<-unique(RSFdata$id)
out<-data.frame()
RSFdata$curr_ndvi_loc<-NA

#before comma, which row to take in spline to match GPS RSFdata, the sub part will say which matching col to take
#output is NDVI value at each pixel
for (j in 1:nrow(RSFdata)){
  RSFdata$curr_ndvi_loc[j]<-spline_ndvi_curr[spline_ndvi_curr$cells==RSFdata$ndvi_cells[j], RSFdata$Column[j]]
}

c <- c(1:16, 19)
names(RSFdata)[17] <- c("ndvi")


###### ------ PERC TREE COVER ------ ######

#read in the data we have so far
dataveg <- SpatialPointsDataFrame(coords = coords, data = RSFdata[,3:17])

#read in rasters from diff years
tree2005 <-raster("2005_PercTree_Final.img") 
tree2006 <-raster("2006_PercTree_Final.img") 
tree2007 <-raster("2007_PercTree_Final.img") 
tree2008 <-raster("2008_PercTree_Final.img") 
tree2009 <-raster("2009_PercTree_Final.img") 
tree2010 <-raster("2010_PercTree_Final.img") 
tree2011 <-raster("2011_PercTree_Final.img") 
tree2012 <-raster("2012_PercTree_Final.img") 
tree2013 <-raster("2013_PercTree_Final.img") 
tree2014 <-raster("2014_PercTree_Final.img") 
tree2015 <-raster("2015_PercTree_Final.img") 
tree2016 <-raster("2016_PercTree_Final.img")                  

#resample so line up properly
tree2005 = resample(tree2005, tree2016, "bilinear")
tree2006 = resample(tree2006, tree2016, "bilinear")
tree2007 = resample(tree2007, tree2016, "bilinear")
tree2008 = resample(tree2008, tree2016, "bilinear")

#crop such that the extents match
ex = extent(tree2016)
tree2005 = crop(tree2005, ex)
tree2006 = crop(tree2006, ex)
tree2007 = crop(tree2007, ex)
tree2008 = crop(tree2008, ex)

#create stack
treestack <- stack(tree2005, tree2006, tree2007, tree2008,
                   tree2009, tree2010, tree2011, tree2012, tree2013, tree2014, tree2015, tree2016)

#set NA value
NAvalue(treestack)<--3000

#from stack to matrix
treestack_mat<-values(treestack)

#Subset to only keep relevant rows
treestack_pix<-extract(treestack[[1]], dataveg, cellnumbers=T)

#this will tell you which row in matrix to extract
RSFdata$tree_cells2<-extract(treestack[[1]], dataveg, cellnumbers=T)[,1]

#tells you which column in matrix to extract
RSFdata$Column<-ifelse(RSFdata$year==2005, 1, ifelse(RSFdata$year==2006, 2, ifelse(RSFdata$year==2007, 3,ifelse(RSFdata$year==2008, 4,
                                                                                                                ifelse(RSFdata$year==2009, 5, ifelse(RSFdata$year==2010, 6,ifelse(RSFdata$year==2011, 7,
                                                                                                                                                                                  ifelse(RSFdata$year==2012, 8,
                                                                                                                                                                                         ifelse(RSFdata$year==2013, 9,
                                                                                                                                                                                                ifelse(RSFdata$year==2014, 10, 
                                                                                                                                                                                                       ifelse(RSFdata$year==2015, 11, 12)))))))))))

RSFdata$percveg_final<-NA

#extracts percent vegetation for each year
for (j in 1:nrow(RSFdata)){
  RSFdata$percveg_final[j]<-treestack_mat[RSFdata$tree_cells[j], RSFdata$Column[j]]
}

cols <- c(1:17,20)
RSFdata <- RSFdata[,cols]

######------ LAND COVER ------######

#read in land use
landuse <- raster("LandUse_ThreeCats.img")

#extract land use for coordinates
landuse <- extract(landuse, coords)
RSFdata$landuse <- landuse

#create 1/0 layers for park, other PA, and community lands (nonPA)
RSFdata$park[(RSFdata$landuse==1)] <- 1
RSFdata$park[(RSFdata$landuse!=1)] <- 0

RSFdata$otherPA[(RSFdata$landuse==2)] <- 1
RSFdata$otherPA[(RSFdata$landuse!=2)] <- 0

RSFdata$nonPA[(RSFdata$landuse==3)] <- 1
RSFdata$nonPA[(RSFdata$landuse!=3)] <- 0

names(RSFdata)[18] <- c("perctree")
write.csv(RSFdata, "RSF_points_wcovs.csv")

######------ CREATE CSV OF POINTS SEPARATED EXACTLY BY TWO HOURS FOR RSF ------######

data_2hr <- read.csv("finaldata_2hronly.csv", header=T)
data <- read.csv("RSF_points_wcovs.csv", header=T)

#need to create a join between the full data (with covariates) and the 2-hr only data (currently not with covariate info)
data$join <- paste(data$id, data$time)
data_2hr$join <- paste(data_2hr$id, data_2hr$date)

join <- inner_join(data_2hr, RSFdata, by="join")
write.csv(join, "RSF_2hrpoints_wcovs.csv")


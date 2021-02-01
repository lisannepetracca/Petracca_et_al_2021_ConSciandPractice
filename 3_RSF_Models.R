# R code associated with 

# Petracca, L.S., Frair, J.L., Bastille-Rousseau, G., Macdonald, D.W., and A.J. Loveridge. 
# 2021. Harassment-induced changes in lion space use as a conflict mitigation tool. 
# Conservation Science and Practice.

# This code represents the running of RSFs (Resource Selection Functions) for lions

# input .csv (RSF_points_wcovs.csv) comes from "2_RSF_Sampling.R"

library(plyr)
library(lme4)
library(corrplot)
library(unmarked)
library(MuMIn)
library(AICcmodavg)
library(sjPlot)

######------ GETTING TO RSF MODELS ------######

#read in data
data <- read.csv("RSF_points_wcovs.csv", header=T)

#create used file
used <- subset(data, used=="1")

#set NA for perctree to 0
data$perctree[is.na(data$perctree)] <- 0

#create correlation plot of continuous covariates
covs <- c("distwater", "distvill", "ndvi", "perctree")
covs <- data[covs]
P <- cor(covs)
corrplot(P, method = "number")

#creating landuse covariate
data$landuse[data$nonPA==1] <- "nonPA"
data$landuse[data$park==1] <- "park"
data$landuse[data$otherPA==1] <- "otherPA"
data$landuse <- as.factor(data$landuse)
data$landuse <- relevel(data$landuse, ref=2)

#adding age/sex class
unique(data$id)
data$class <- "male"
data$class[(data$id=="NgamoUrchin" | data$id=="Frisky" |
              data$id=="Backpans" | data$id=="Inkosikasi" |
              data$id=="OldGuvalala" | data$id=="SpiceJunior" )]  <- "female"
unique(data$class)
head(data)

data$treated	<-	"protected"																								
data$treated[(data$id=="NgamoUrchin"	|	data$id=="Frisky"	|	
                data$id=="Jericho"	|	data$id=="Vanilla"	|																							
                data$id=="Chikarubi"	|	data$id=="Kakori"	)]	<-	"at-risk"	
unique(data$treated)

#relevel before/after
data$after <- as.factor(data$after)
data$after <- relevel(data$after, ref="before")

#making "after" numeric
data$after2 <- data$after
data$after2 <- as.numeric(revalue(data$after2, c("after"="1", "before"="0")))

#taking log of distance to waterhole and distance to household
data$distwaterlog <- log(data$distwater + 1)
data$distvilllog <- log(data$distvill + 1)

#making ndvi from -1 to 1
data$ndvi <- (data$ndvi - min(data$ndvi))/(max(data$ndvi)-min(data$ndvi))

#dividing perctree by 100
data$perctree <- data$perctree/100

#creating wet and dry data
wet <- subset(data, wet==1)
dry <- subset(data, wet==0)

#number of points for each lion and season
data.frame(table(wet$id))
data.frame(table(dry$id))

#creating wet and dry model names
wet_models <- split(wet, wet$id, drop=TRUE)
names(wet_models)

dry_models <- split(dry, dry$id, drop=TRUE)
names(dry_models)

wetmod <- list()
drymod <- list()

######------ RUNNING RSFS, AND USING AFTER AS BINARY VARIABLE TO PLOT DIFFERENCE BEFORE:AFTER ------######

#running wet season models
wetmod[[1]] <- glmer(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2 + (1|year), data = wet_models[[1]], family = binomial)
wetmod[[2]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) + perctree:distvilllog +  perctree:distvilllog:after2,	data	=	wet_models[[2]],	family	=	binomial)					
wetmod[[3]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) + perctree:distvilllog + perctree:distvilllog:after2,	data	=	wet_models[[3]],	family	=	binomial)		
wetmod[[4]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	wet_models[[6]],	family	=	binomial)						
wetmod[[5]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	wet_models[[7]],	family	=	binomial)		
wetmod[[6]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	wet_models[[10]],	family	=	binomial)					
wetmod[[7]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	wet_models[[11]],	family	=	binomial)		
wetmod[[8]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	wet_models[[12]],	family	=	binomial)					
wetmod[[9]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	wet_models[[13]],	family	=	binomial)					
wetmod[[10]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	wet_models[[15]],	family	=	binomial)					
wetmod[[11]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	wet_models[[16]],	family	=	binomial)		
wetmod[[12]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	wet_models[[17]],	family	=	binomial)					

#running dry season models
drymod[[1]] <- glmer(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2 + (1|year), data = dry_models[[1]], family = binomial)
drymod[[2]]	<-  glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2, data	=	dry_models[[2]],	family =	binomial)					
drymod[[3]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	dry_models[[3]],	family	=	binomial)		
drymod[[4]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	dry_models[[4]],	family	=	binomial)					
drymod[[5]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	dry_models[[6]],	family	=	binomial)						
drymod[[6]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	dry_models[[8]],	family	=	binomial)					
drymod[[7]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	dry_models[[10]],	family	=	binomial)					
drymod[[8]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	dry_models[[11]],	family	=	binomial)		
drymod[[9]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	dry_models[[12]],	family	=	binomial)					
drymod[[10]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	dry_models[[13]],	family	=	binomial)					
drymod[[11]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	dry_models[[15]],	family	=	binomial)					
drymod[[12]]	<-	glm(used ~ distwaterlog + distvilllog + distvilllog:after2 + perctree + ndvi + I(ndvi^2) +  perctree:distvilllog + perctree:distvilllog:after2,	data	=	dry_models[[16]],	family	=	binomial)		

#getting coefficients for wet and dry seasons in data frame
wetdat <- data.frame(matrix(nrow=12, ncol=2))
for (i in 2:12){
  wetdat[1,1] <- fixef(wetmod[[1]])[7]  
  wetdat[1,2] <- sqrt(diag(vcov(wetmod[[1]])))[7]
  wetdat[i,1] <- summary(wetmod[[i]])$coef[7,1]
  wetdat[i,2] <- summary(wetmod[[i]])$coef[7,2]
}

drydat <- data.frame(matrix(nrow=12, ncol=2))
for (i in 2:12){
  drydat[1,1] <- fixef(drymod[[1]])[7]  
  drydat[1,2] <- sqrt(diag(vcov(drymod[[1]])))[7]
  drydat[i,1] <- summary(drymod[[i]])$coef[7,1]
  drydat[i,2] <- summary(drymod[[i]])$coef[7,2]
}

wetdat$season <- "wet"
drydat$season <- "dry"

#combining output from wet and dry seasons
alldat <- rbind(wetdat, drydat)
#giving column names
colnames(alldat) <- c("villdiff", "villse", "season") 
#adding lion id
alldat$lion <- c("Backpans", "Bhubesi", "Cecil", "Frisky", "Goose", "Kakori", "Lucky", "NgamoUrchin", "OldGuvalala", "SpiceJunior", 
                 "Tommy", "Vanilla", "Backpans", "Bhubesi", "Cecil", "Chikarubi", "Frisky", "Inkosikazi", "Kakori", "Lucky", "NgamoUrchin", "OldGuvalala", 
                 "SpiceJunior", "Tommy")

#adding male/female
alldat$sex <- "male"
alldat$sex[(alldat$lion=="NgamoUrchin" | alldat$lion=="Frisky" |
              alldat$lion=="Backpans" | alldat$lion=="Inkosikazi" |
              alldat$lion=="OldGuvalala" | alldat$lion=="SpiceJunior" )]  <- "female"

#adding group
alldat$treated	<-	"protected"																								
alldat$treated[(alldat$lion=="NgamoUrchin"	|	alldat$lion=="Frisky"	|	
                  alldat$lion=="Vanilla"	|																							
                  alldat$lion=="Chikarubi"	|	alldat$lion=="Kakori"	)]	<-	"at-risk"	

#add lion name
alldat$lion = factor(alldat$lion,levels=c("Frisky", "NgamoUrchin", "Chikarubi", "Vanilla", 
                                          "Kakori", "Backpans", "Inkosikazi",
                                          "OldGuvalala", "SpiceJunior", "Bhubesi", "Cecil","Goose", "Lucky", 
                                          "Tommy"))

#create season for facet plot
alldat$season <- as.factor(alldat$season)
alldat$season <- relevel(alldat$season, ref="wet")

#creating plot of difference in selection before vs after by lion and season
p <- ggplot(alldat, aes(villdiff, color=treated))
p + geom_point(x=alldat$villdiff, y=0.5,size=2.5) + facet_grid(lion~season)+ xlim(-.25,.5)+ #ylim(0,1)+
  geom_errorbarh(aes(xmax = villdiff + 2*villse, xmin = villdiff - 2*villse, y=0.5),size=0.5)+
  geom_vline(aes(xintercept=0),linetype=2)+
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())

ggsave(
  "Figure5.pdf",
  plot = last_plot(),
  path = "Figures")


# R code associated with 

# Petracca, L.S., Frair, J.L., Bastille-Rousseau, G., Macdonald, D.W., and A.J. Loveridge. 
# 2021. Harassment-induced changes in lion space use as a conflict mitigation tool. 
# Conservation Science and Practice.

# This code represents an analysis of home range shift by lions, and an analysis of inter-season movements

# Produced Figures 2 and 3 in paper

library(here)
library(raster)
library(sf)
library(ggpubr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(car)
library(rgeos)
library(coin)

###### ------ HOME RANGE OVERLAP W LANDUSE  ------ ######

here()

#reading in rasters for national park, other protected areas, and community lands (nonPA)
NP <- raster("NP_only.img")
OtherPA <- raster("OtherPA_only.img")
NonPA <- raster("NonPA_only.img")

#reading in 95% kernel HRs
HR <- st_read("seasonalHR_95kernel.shp")
HR_spatial <- as(HR, "Spatial")

#aggregating the rasters to 200m res
NP_200m <- aggregate(NP, fact=20)
OtherPA_200m <- aggregate(OtherPA, fact=20)
NonPA_200m <- aggregate(NonPA, fact=20)

#extract % of each category by HR
HR_Park <- extract(NP_200m, HR_spatial, fun=mean)
HR_OtherPA <- extract(OtherPA_200m, HR_spatial, fun=mean)
HR_NonPA <- extract(NonPA_200m, HR_spatial, fun=mean)

#this information became the below .csv (and is present in Data folder)
hrdata <- read.csv("hr_overlap.csv", header=T)

#add male/female, protected/at-risk, before/after
hrdata$sex <- "male"
hrdata$sex[(hrdata$lion=="NgamoUrchin" | hrdata$lion=="Frisky" |
              hrdata$lion=="Backpans" | hrdata$lion=="Inkosikasi" |
              hrdata$lion=="OldGuvalala" | hrdata$lion=="SpiceJunior" )]  <- "female"

hrdata$treated <- "protected"
hrdata$treated[(hrdata$lion=="NgamoUrchin" | hrdata$lion=="Frisky" |
                  hrdata$lion=="Jericho" | hrdata$lion=="Vanilla" |
                  hrdata$lion=="Chikarubi" | hrdata$lion=="Kakori" )]  <- "at-risk"

hrdata <- hrdata %>% separate(FullID, c(NA, "B", "C"))
hrdata$season <- paste(hrdata$B, hrdata$C)
hrdata$period <- "before"
hrdata$period[(hrdata$season=="Dry 2012" |
                 hrdata$season=="Wet 2013" | hrdata$season=="Dry 2013" |
                 hrdata$season=="Wet 2014" | hrdata$season=="Dry 2014" |
                 hrdata$season=="Wet 2015" | hrdata$season=="Dry 2015" |
                 hrdata$season=="Wet 2016" )]  <- "after"
head(hrdata)

#summarize some metrics and save as .csv
summary_prop <- hrdata %>% group_by(lion,period) %>% summarise(mean_NP = median(NP),
                                                               mean_otherPA = median(otherPA),
                                                               mean_nonPA = median(nonPA),n = n())
summary_prop <- as.data.frame(summary_prop)
write.csv(summary_prop, "summarized_proportions_indiv.csv")

hrdata$period <- as.factor(hrdata$period)
hrdata$period <- relevel(hrdata$period, ref="before")

#summary table
table(hrdata$sex, hrdata$treated, hrdata$period)

#exclude Raah and Jericho
hrdata<- hrdata %>% filter(lion!="Raah" & lion!="Jericho")

#assign sex, at-risk vs protected
male <- subset(hrdata, sex=="male")
female <- subset(hrdata, sex=="female")

maleT <- male %>% filter(treated=="at-risk")
femaleT <- female %>% filter(treated=="at-risk")
maleC <- male %>% filter(treated=="protected")
femaleC <- female %>% filter(treated=="protected")

#one-way ANOVAS to test if there is a difference before and after re: % overlap with national parks and community lands
Lme.mod <- lme(NP ~ period, random=~1|lion,
               correlation=corCompSymm(form=~1|lion),
               data=maleT)
anova(Lme.mod)
Lme.mod <- lme(NP ~ period, random=~1|lion,
               correlation=corCompSymm(form=~1|lion),
               data=maleC)
anova(Lme.mod)
Lme.mod <- lme(NP ~ period, random=~1|lion,
               correlation=corCompSymm(form=~1|lion),
               data=femaleT)
anova(Lme.mod)
Lme.mod <- lme(NP ~ period, random=~1|lion,
               correlation=corCompSymm(form=~1|lion),
               data=femaleC)
anova(Lme.mod)

Lme.mod <- lme(otherPA ~ period, random=~1|lion,
               correlation=corCompSymm(form=~1|lion),
               data=maleT)
anova(Lme.mod)
Lme.mod <- lme(otherPA ~ period, random=~1|lion,
               correlation=corCompSymm(form=~1|lion),
               data=maleC)
anova(Lme.mod)
Lme.mod <- lme(otherPA ~ period, random=~1|lion,
               correlation=corCompSymm(form=~1|lion),
               data=femaleT)
anova(Lme.mod)
Lme.mod <- lme(otherPA ~ period, random=~1|lion,
               correlation=corCompSymm(form=~1|lion),
               data=femaleC)
anova(Lme.mod)

#this produces Figure 3 (boxplot of before vs after percent overlap)
NP <- ggboxplot(hrdata, x = "treated", y = "NP", color="period") + facet_grid(1~sex)
nonPA <- ggboxplot(hrdata, x = "treated", y = "nonPA", color="period") + facet_grid(1~sex)

library(cowplot)
library(ggplot2)
plot_grid(NP, nonPA, labels = c('A', 'B'), ncol=1)

ggsave(
  "Proportion_Plot.pdf",
  plot = last_plot(),
  path = "Figures")


###### ------ ANALYSIS OF INTER-SEASON MOVEMENTS  ------ ######

#read in centroids (this .csv is in "Data" folder)
#"dist_final" represents distances between consecutive seasonal home ranges
cent <- read.csv("G:/My Drive/GitHub/Petracca_et_al_2021_ConSciandPractice/Data/centroids_core.csv", header=T)
cent <- cent[!is.na(cent$dist_final),]

#remove Raah and Jericho
cent <- cent %>% filter (id!= "Raah" & id!="Jericho")
unique(cent$id)

#add male/female, treated/control, before/after
cent$sex <- "male"
cent$sex[(cent$id=="NgamoUrchin" | cent$id=="Frisky" |
             cent$id=="Backpans" | cent$id=="Inkosikasi" |
             cent$id=="OldGuvalala" | cent$id=="SpiceJunior" )]  <- "female"

cent$treated <- "protected"
cent$treated[(cent$id=="NgamoUrchin" | cent$id=="Frisky" |
                 cent$id=="Vanilla" |
                 cent$id=="Chikarubi" | cent$id=="Kakori" )]  <- "at-risk"

#assign before/after
cent <- cent %>% separate(fullid, c(NA, "B", "C"))
cent$season <- paste(cent$B, cent$C)
cent$period <- "before"
cent$period[(cent$season=="Dry 2012" |
                cent$season=="Wet 2013" | cent$season=="Dry 2013" |
                cent$season=="Wet 2014" | cent$season=="Dry 2014" |
                cent$season=="Wet 2015" | cent$season=="Dry 2015" |
                cent$season=="Wet 2016" )]  <- "after"

#make "before" reference level
cent$period <- as.factor(cent$period)
cent$period <- relevel(cent$period, ref="before")

#make factors
cent$sex <- as.factor(cent$sex)
cent$treated <- as.factor(cent$treated)

#doing wilcoxon unpaired rank test
test <- wilcox_test(dist_final ~ sex, data = cent,
                    distribution = "exact", 
                    conf.int = TRUE)

#below code makes Figure 2 (violin plot of inter-season movements)
f <- ggplot(cent, aes(period, dist_final))
f + geom_violin() +facet_grid(rows = vars(sex), cols = vars(treated))+stat_summary(fun.y=mean, geom="point",size=2)
ggsave(
  "Violin_Plot.pdf",
  plot = last_plot(),
  path = "Figures")

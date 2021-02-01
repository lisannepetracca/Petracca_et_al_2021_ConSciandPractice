# R code associated with 

# Petracca, L.S., Frair, J.L., Bastille-Rousseau, G., Macdonald, D.W., and A.J. Loveridge. 
# 2021. Harassment-induced changes in lion space use as a conflict mitigation tool. 
# Conservation Science and Practice.

# This code read in all data with 2-hr fix interval, and plotted kernel density of GPS points by hour before vs. after

# Produced Figure 4 in paper

library(here)
library(dplyr)
library(plyr)
library(RColorBrewer)
library(ggplot2)

here()
#this .csv comes from end of Code 2 ("RSF_Sampling")
data <- read.csv("RSF_2hrpoints_wcovs.csv", header=T)

#subset to used points only
data <- subset(data, used==1)

#get rid of Jericho and Raah because these lions weren't included in final analysis
data <-data %>%  filter(  
  id.x!="Jericho" & id.x!="Raah")

#set "before" as reference level
levels(as.factor(data$after))
data$after <- relevel(as.factor(data$after), ref= "before")

#"close" points are those that are in community lands that are 2000 m from households
close <- subset(data, nonPA==1)
close <- subset(close, distvill<2000)

#summarize number of close points by lion
y = count(close, 'id.x')

#setting groups of "close" points and "all" points
close$group <- "close"
data$group <- "all"
all <- rbind(close, data)

#separate close points by season
closewet <- close %>% filter(wet==1)
closedry <- close %>% filter(wet==0)

dim(closewet)
dim(closedry) #interesting that 1139 of points were from wet season, only 243 from dry

c <- ggplot(all, aes(hour, color=group, fill=group)) + geom_density(kernel = "gaussian",alpha=.2, size=1) + facet_grid(1~after)+
  scale_fill_brewer(palette="Accent")+
  scale_color_brewer(palette="Accent")
c
ggsave(
  "Figures/ActivityPattern.pdf",
  plot = last_plot())

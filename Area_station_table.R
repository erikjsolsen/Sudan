#' @title Analysis of catch data from coastal surveys of Sudan 2012-2013
#' @description Table 1 of manuscript 
#' @author Erik Olsen
#' @note Created Augh 2019
#' 


#' 
#' LIBRARIES AND SOURCES
#' --------------------------
#' 

library(reshape2)
library(maps)
library(mapdata)
library(ggplot2)
library(RColorBrewer)
library(sp)
library(rgdal)
library(rgeos)
# plyr mus be loaded befor dplyr for both to work
library(plyr) 
library(dplyr)
library(tidyr)
library(maptools)
library(marmap)
library(classInt)

setwd("~/github/sudan")

source('multiplot function.R', encoding='UTF-8')

#' CATCH DATA
#' --------------------------
catch<-read.csv2("catch2.csv")
catch$survey<-as.factor(catch$survey)
catch$CPUEw<-catch$weight/catch$Fhrs
catch$CPUEn<-catch$number/catch$Fhrs

#' Add Month. Year survey column
catch$survey_m<-as.character(catch$survey)
catch$survey_m<-replace(catch$survey_m, grep("2012901", catch$survey_m), "Survey 1: Nov. 2012")
catch$survey_m<-replace(catch$survey_m, grep("2013002", catch$survey_m), "Survey 2: May 2013")
catch$survey_m<-replace(catch$survey_m, grep("2013005", catch$survey_m), "Survey 3: Nov. 2013")

#' SUDAN MAP DATA
#' ---------------------------
world<- map_data("worldHires", c("Sudan", "Ethiopia")) 
world$survey_m<-c("Survey 1: Nov. 2012")
world2<-world
world2$survey_m<-c("Survey 2: May 2013")
world3<-world
world3$survey_m<-c("Survey 3: Nov. 2013")
world<-rbind(world, world2, world3)

#'  SPATIAL POLYGONS FOR MANAGEMENT AREAS
#' -------------------------------------------
setwd("~/github/sudan/sudan_management_areas/")
ManageAreas<-readOGR(dsn = ".", "sudan_regions")
setwd("~/github/sudan")

#' setting correct projection LAT LON
ManageAreas <- spTransform(ManageAreas, CRS("+proj=longlat +ellps=GRS80"))

#' Create  area map with numbers for each region
ManageAreas.f<-fortify(ManageAreas, region="id") #creates X - Y points of the polygons

cnames <- aggregate(cbind(long, lat) ~ id, data=ManageAreas.f, FUN=function(x)mean(range(x)))
cnames$id<-c(1:7)



#' ALLOCATING CATCH POSITIONS TO MANAGEMENT AREAS
#' ---------------------------
#' use 'over' method from SP package

#' Create spatial.points data base from catch

catch.points<- SpatialPoints(catch[8:9])
proj4string(catch.points) <- proj4string(ManageAreas)
catch.areas<-over(catch.points, ManageAreas)
catch.areas$id <- as.integer(as.character(catch.areas$id))

#' correct for errors in numbering areas
catch.areas[grep("3|4|5|6", catch.areas$id),]$id <- catch.areas[grep("3|4|5|6", catch.areas$id),]$id-1
catch.areas[grep("8", catch.areas$id),]$id <- catch.areas[grep("8", catch.areas$id),]$id-2
catch.areas$id <- as.factor(catch.areas$id)
catch <- cbind(catch, catch.areas[1:2])


#' add area no to stations outside of managment area polygons
new_df <- subset(catch, is.na(catch$name))
new_df$id <- as.factor(new_df$Area)
new_df[1,]$name <- c("Arakiai")
new_df[2:8,]$name <- c("Suakin Archipelago")
new_df[9:16,]$name <- c("Donganab")

catch <-rbind(subset(catch, !is.na(catch$name)), new_df)


#' Change family names to Upper & Lower case
catch$FamGroup2 <- paste(toupper(substr(catch$FamGroup, 1, 1)), tolower(substr(catch$FamGroup, 2, 20)), sep="")

catch$FamGroup2 <- as.factor(catch$FamGroup2)


#' TABLE OF SAMPLING EFFORT PR. MANAGEMENT AREA
#' (new for resubmission to PlosOne)
#' --------------------------------------------

s <- unique(catch$survey)
a <- as.factor(1:7)
ay_info <- data.frame(survey=integer(), id=integer(), Ntraps=integer(), Nhl=integer(), NGn=integer(), TBhrs=double(), HLhrs=double(), GNhrs=double(), DepthAvg=double(), DepthMax=double(), DepthMin=double()) 
cn <- colnames(ay_info)
# depth for TB only (GN are at the surface, and HL not measured)

for (i in 1:length(s)) { 
  y <- subset(catch, survey==s[i])
  for (j in 1: length(a)){
    ay_1 <- data.frame(survey=integer(), id=integer(), Ntraps=integer(), Nhl=integer(), NGn=integer(), TBhrs=double(), HLhrs=double(), GNhrs=double(), DepthAvg=double(), DepthMax=double(), DepthMin=double())
    ya <- subset(y, id==a[j])
    ay_1 <- rbind(ay_1, c(
      as.integer(as.character(ya$survey[1])), 
      as.integer(as.character(ya$id[1])), 
      length(unique(subset(ya, gear=="TB")$station)),  
      length(unique(subset(ya, gear=="HL")$station)), 
      length(unique(subset(ya, gear=="GN")$station)), 
      sum(subset(ya, gear=="TB")[!duplicated(ya$station),]$Fhrs, na.rm=TRUE),  
      sum(subset(ya, gear=="HL")[!duplicated(ya$station),]$Fhrs, na.rm=TRUE), 
      sum(subset(ya, gear=="GN")[!duplicated(ya$station),]$Fhrs, na.rm=TRUE), 
      mean(subset(ya, gear=="TB")[!duplicated(ya$station),]$bdep, na.rm=TRUE), 
      max(subset(ya, gear=="TB")[!duplicated(ya$station),]$bdep, na.rm=TRUE), 
      min(subset(ya, gear=="TB")[!duplicated(ya$station),]$bdep, na.rm=TRUE)    ))
    colnames(ay_1) <- cn
    ay_info <- rbind(ay_info, ay_1)
    colnames(ay_info) <- cn
  }
}
write.csv2(ay_info, "Area_Station_table.csv")





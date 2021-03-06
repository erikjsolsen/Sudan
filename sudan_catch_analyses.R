#' @title Analysis of catch data from coastal surveys of Sudan 2012-2013
#' @description Figures 1 - 9 of manuscript 
#' @author Erik Olsen
#' @note Revised Oct 2017
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

#' Species and L-W data
species.list<-read.csv2("Species_table.csv")
length.weight<-read.csv2("LW_pilot_sudan12_15.csv")


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


#' DEFINE VARIABLES
#' ---------------------------
#' position of Port Sudan
ps<-rbind(data.frame(x=37.21709967,y=19.600512, group=1, survey_m=c("Survey 1: Nov. 2012")),data.frame(x=37.21709967,y=19.600512, group=1, survey_m=c("Survey 2: May 2013") ),data.frame(x=37.21709967,y=19.600512, group=1, survey_m=c("Survey 3: Nov. 2013") ))



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

#' SELECTING AND MANIPULATING CATCH DATA
#' -----------------------------------
#' selecting only stations with catch
catch1<-subset(catch, Sci_name!="NO CATCH")

#' removes stations shorter than 1 hour
catch1<-subset(catch1, Fhrs>1)
catch.traps<-subset(catch1, gear!="GN")

#' reordering the species groups
fam.names<-c("Acanthuridae", "Carangidae", "Chirocentridae", "Lethrinidae", "Lutjanidae", "Scombridae",     "Serranidae", "Other spp" )  

levels(catch1$FamGroup2)<-fam.names


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

#' ANALYSIS OF CATCHES BY DEPTH
#' - plot catch vs. depth
#' - Plot catch of single species (10 most common) vs. depth
#' - investigate statistical influence of depth (e.g GAM model)





#' BATHYMETRIC MAP OF SUDAN WITH MANAGEMENT AREAS (FIGURE 1)
#' ----------------------------------------------
sudan<-readGEBCO.bathy("./sudan_bathymetry/sudan.nc")
blues <- colorRampPalette(c("midnightblue", "deepskyblue3", "deepskyblue1", "cadetblue3", "cadetblue1", "darkseagreen1", "white"))

tiff(file="fig 1 sudan_bathy.tiff", width=1900, height=3000, res=400, pointsize=10, compression=c("none"))

plot(sudan, land = TRUE, n = 10, image = TRUE,
     bpal = list(c(min(sudan), -20, "midnightblue", "blue", "lightblue3"),
                 c(-20, 0, "lightblue1", "aquamarine1"),
                 c(0, max(sudan), "gray90", "gray50")), 
     deep = c(-3000, -500, 0),
     shallow = c(-500, -10, 0),
     step = c(500, 200, 0),
     lwd = c(0.8, 0.8, 1), lty = c(1, 1, 1),
     col = c("lightgrey", "gray15", "black"),
     drawlabel = c(TRUE, TRUE, TRUE))


scaleBathy(sudan, deg=0.5 ,x="bottomleft", inset=5)

points(37.21709967, 19.600512, pch=19)

text(37.21709967, 19.600512,"Port \nSudan", adj=c(1.2,0), font=2)
text(37.2, 21.7, "1. Marsas north \nof Dongonab", cex=0.9)
text(37.4, 20.92, "2. Dongonab", cex=0.9)
text(37.38, 20.3, "3. Arakiai", cex=0.9)
text(37.4, 19.9, "4. Port \nSudan", cex=0.9)
text(37.6, 19, "5. Suakin", cex=0.9)
text(37.9, 19.3, "6. Suakin \narchipelago", cex=0.9)
text(38.3, 18.5, "7. Agig", cex=0.9)



ms<-c(1,3,4,5,6,7,8)
for (i in 1:7){
  ma<-subset(ManageAreas.f, id==ms[i])
  polygon(ma$long, ma$lat, lwd=2)  
}
dev.off()


#' PLOTTING CATCH POSITIONS BY SURVEY (FIGURE 2)
#' ----------------------------------
sudan.map<-ggplot(catch, aes(x=lon, y=lat, group=survey_m)) + geom_point(shape="+", size=6, aes(group=survey_m, colour= survey_m)) + facet_wrap(~survey_m, ncol=3) + geom_polygon(data=world, colour="gray35", fill="gray85", aes(x=long, y=lat, group=survey_m)) +  coord_cartesian(xlim = c(36.5, 39), ylim=c(17.7, 22.5)) + theme_classic() + geom_point(data=ps, size=5, colour="gray35", aes(x=x, y=y)) + geom_text(data=ps, label="PZU", size=5,  hjust=1, vjust=-1.2,  aes(x=x, y=y, group=survey_m)) +scale_colour_brewer(type = "seq", palette = "Dark2", name="Survey month & year", labels=c("Nov. 2012", "May. 2013", "Nov. 2013")) + theme(legend.position="none") + xlab("Longitude") + ylab("Latitude")
sudan.map
ggsave("Fig 2 sudan_stations_12_13_facet.tiff")




####### DON'T USE ####
#' ANOVA OF CATCHES BY GEAR AND MANAGEMENT AREAS (CPUE by Weight)
#' ---------------------------

#ANOVA CPUEw
catch.df<-subset(catch1, number>0)
catch.df$Area<-factor(catch.df$Area, labels = c(1:7))
catch.anova<-lm(CPUEw ~ Area, data = catch.df)
summary(catch.anova)
anova(catch.anova)
confint(catch.anova)
# significant for area 2 and area 6

#ANOVA by year and area
catch.anova2<-lm(CPUEw ~  survey + Area, data = catch.df)
#survey May 2013 was significantly different than the others
summary(catch.anova2)
anova(catch.anova2)
#significant for survey and area
a1 <- aov(CPUEw ~  survey + Area, data = catch.df)
posthoc <- TukeyHSD(x=a1, 'survey', conf.level=0.95)
posthoc
# survey May 2012 was significantly different in wCPUE than Nov 2012 and Nov 2013
posthoc <- TukeyHSD(x=a1, 'Area', conf.level=0.95)
posthoc
# not significan for any area-area combination

##### END DON'T RUN


# Statistical comparison of catch rates
# Weight - CPUE

#' Kruskal - Wallis  (non.parametric alternative to ANOVA)

group_by(catch, id) %>% 
  summarise(
    count = n(),
    mean = mean(CPUEw, na.rm = TRUE),
    sd = sd(CPUEw, na.rm = TRUE),
    median = median(CPUEw, na.rm = TRUE),
    IQR = IQR(CPUEw, na.rm = TRUE)
  )

# boxplot of catch pr mgmt areas
library("ggpubr")
mypalette<-brewer.pal(7,"Accent")
ggboxplot(catch, x = "id", y = "CPUEw", 
          color = "id", palette = mypalette,
          order = c("1", "2", "3", "4", "5", "6", "7"),
          ylab = "CPUE kg/hr", xlab = "management areas")
ggsave("boxplot_mgmt_areas_cpue.png")

# Kruskal -Wallis test - By areas
kruskal.test(CPUEw ~ id, data = catch)
# by survey
kruskal.test(CPUEn ~ survey, data = catch)

# significant difference

# Post-hoc
# Pair.wise Wilcoxon test

pairwise.wilcox.test(catch$CPUEw, catch$id, p.adjust.method = "BH")

# Dunn test for multiple comparisons
library("FSA")

DT <- dunnTest(CPUEw ~id, data=catch, method="bh")
DT

dunnTest(CPUEw ~id, data=catch)
dunnTest(CPUEw ~id, data=catch, method="bonferroni")


# Numbers - CPUE

#' Kruskal - Wallis  (non.parametric alternative to ANOVA)

group_by(catch, survey) %>% 
  summarise(
    count = n(),
    mean = mean(CPUEn, na.rm = TRUE),
    sd = sd(CPUEn, na.rm = TRUE),
    median = median(CPUEn, na.rm = TRUE),
    IQR = IQR(CPUEn, na.rm = TRUE)
  )

# boxplot of catch pr mgmt areas
library("ggpubr")
mypalette<-brewer.pal(7,"Accent")
ggboxplot(catch, x = "survey", y = "CPUEn", 
          color = "survey", palette = mypalette,
          ylab = "CPUE no fish/hr", xlab = "surveys")
ggsave("boxplot_surveys.png")

# Kruskal -Wallis test
kruskal.test(CPUEn ~ id, data = catch)


# by survey
kruskal.test(CPUEn ~ survey, data = catch)

# significant difference

# Post-hoc
# Pair.wise Wilcoxon test

pairwise.wilcox.test(catch$CPUEn, catch$survey, p.adjust.method = "BH")

# Dunn test for multiple comparisons
library("FSA")

DT <- dunnTest(CPUEn ~survey, data=catch, method="bh")
DT




#' CPUE (weight) PLOT BY GEAR TYPE (FIGURE 3)
#' ------------------------------
catch.3gear<-subset(catch, gear=="GN" | gear=="HL" | gear =="TB")

gear.plot <- ggplot(catch.3gear, aes(FamGroup2, fill=survey_m)) + geom_bar(binwidt=0.2, aes( weight=CPUEw)) +theme_bw() +facet_grid(gear ~ survey) + coord_flip() +xlab("") + ylab("kg fish/hour fishing")  + guides(fill=guide_legend(title="Survey")) 
gear.plot + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank())
ggsave("Fig 3 gearplot.tiff")
# ggsave("Fig 3 gearplot.pdf")




#' CPUE (weight) PLOT BY MANAGEMENT AREAS (FIGURE 4)
#' ---------------------------------
lat.plot<-ggplot(catch1, aes(Area, fill=FamGroup2)) + geom_bar(binwidt=0.2, aes( weight=CPUEw)) +theme_classic() + scale_x_reverse(breaks=c(1,2,3,4,5,6,7)) 
lat.plot + scale_fill_brewer(palette="RdYlBu", name="Family groups") + coord_flip() + theme(legend.title = element_text(size=14, face="bold")) + theme(legend.text = element_text(size=12)) + ylab("CPUE weight (kg) of fish/hr fishing") + xlab("Management areas from N (1) to S (7)") 
#+ theme(axis.text.x = element_blank()) 
#lat.plot
#figure 
ggsave("Fig 4 management area CPUE weight.tiff")
ggsave("Fig 4 management area CPUE weight.pdf")


#' GRIDDING CATCH DATA (CPUE by weight) to 0.1 x 0.1 DEGREE GRID OF SUDAN
#' FIGURES 5, 6, 7, and 8
#' ---------------------------------

# set grid X and Y limits
xvals <- seq(36.5, 38.8, by=0.1)
yvals <- seq(18.3, 22.1, by=0.1)

#kategorizing the geom-data
catch$x<-round(catch$lon, 1)
catch$y<-round(catch$lat, 1)

# TRAPS, GILLNET and HAND-LINE CPUE data set
#remove NO CATCH data
catch<-subset(catch, Fam_name!="NO CATCH")
TBcpue<-subset(catch, gear=="TB")
GNcpue<-subset(catch, gear=="GN")
HLcpue<-subset(catch, gear=="HL")

#select top5 fish 
fish5<-c("Lutjanus bohar", "Lutjanus gibbus", "Lethrinus lentjan", "Lethrinus mahsena", "Sargocentron spiniferum")
topfish<-subset(catch, Sci_name==fish5)

#calculate Mean and Total
#Must first detach plyr to use the 'dplyr' summarise function
detach("package:plyr", unload=TRUE) 

# total catch
catch_pr_cell<-as.data.frame(group_by(catch,x,y) %>% summarise(mean=mean(CPUEw),total=sum(CPUEw), first(survey_m)))
colnames(catch_pr_cell)[5] <- c("survey_m")

#catch of top 5 fish
top_catch_pr_cell<-as.data.frame(group_by(topfish,Sci_name, survey,x,y) %>%  summarise(mean=mean(CPUEw),total=sum(CPUEw), first(survey_m)))
colnames(top_catch_pr_cell)[7] <- c("survey_m")

#catch by gear type
TBspecies_catch_pr_cell<-as.data.frame(group_by(TBcpue,survey,Fam_name2,x,y) %>% summarise(mean=mean(CPUEw), total=sum(CPUEw), first(survey_m)))
colnames(TBspecies_catch_pr_cell)[7] <- c("survey_m")

HLspecies_catch_pr_cell<-as.data.frame(group_by(HLcpue,survey,Fam_name2,x,y) %>%  summarise(mean=mean(CPUEw), total=sum(CPUEw), first(survey_m)))
colnames(HLspecies_catch_pr_cell)[7] <- c("survey_m")

GNspecies_catch_pr_cell<-as.data.frame(group_by(GNcpue,survey,Fam_name2,x,y) %>%  summarise(mean=mean(CPUEw), total=sum(CPUEw), first(survey_m)))
colnames(GNspecies_catch_pr_cell)[7] <- c("survey_m")

#weight categories
#ALL
ALL_brks<-classIntervals(catch_pr_cell$mean, n=7, style="fixed", fixedBreaks=c(0, 0.02, 0.05, 0.1, 0.5, 1, 5, 18.2)) #define categories
ALL_brks<-round(ALL_brks$brks,digits=3) #round
ALL_catVar<-findInterval(catch_pr_cell$mean, ALL_brks, all.inside=TRUE) #assign categories
catch_pr_cell$All_catVar<-ALL_catVar

#TOP 5 species
top_brks<-classIntervals(top_catch_pr_cell$mean, n=7, style="fixed", fixedBreaks=c(0, 0.02, 0.05, 0.1, 0.5, 1, 5, 18.2)) #define categories
top_brks<-round(top_brks$brks,digits=3) #round
top_catVar<-findInterval(top_catch_pr_cell$mean, top_brks, all.inside=TRUE) #assign categories
top_catch_pr_cell$top_catVar<- top_catVar

#TB
TB_brks<-classIntervals(TBspecies_catch_pr_cell$mean, n=7, style="fixed", fixedBreaks=c(0, 0.02, 0.05, 0.1, 0.5, 1, 5, 18.2)) #define categories
TB_brks <- round(TB_brks$brks,digits=3) #round
TB_catVar<-findInterval(TBspecies_catch_pr_cell$mean, TB_brks, all.inside=TRUE) #assign categories
TBspecies_catch_pr_cell$TB_catVar<-TB_catVar

#HL
HL_brks<-classIntervals(HLspecies_catch_pr_cell$mean, n=7, style="fixed", fixedBreaks=c(0, 0.02, 0.05, 0.1, 0.5, 1, 5, 18.2)) #define categories
HL_brks <- round(HL_brks$brks,digits=3) #round
HL_catVar<-findInterval(HLspecies_catch_pr_cell$mean, HL_brks, all.inside=TRUE) #assign categories
HLspecies_catch_pr_cell$HL_catVar<- HL_catVar

#GN
GN_brks<-classIntervals(GNspecies_catch_pr_cell$mean, n=7, style="fixed", fixedBreaks=c(0, 0.02, 0.05, 0.1, 0.5, 1, 5, 18.2)) #define categories
GN_brks <- round(GN_brks$brks,digits=3) #round
GN_catVar<-findInterval(GNspecies_catch_pr_cell$mean, GN_brks, all.inside=TRUE) #assign categories
GNspecies_catch_pr_cell$GN_catVar<- GN_catVar

#create subset of catches >0 
catch_pr_cell1<-subset(catch_pr_cell, mean>=0)

#creata a plot-data set
plotdata<-merge(catch_pr_cell,expand.grid(x=xvals,y=yvals),all.y=T)
#select non NA lines
plotdata1<-subset(plotdata, mean>=0)

#create plot-data for species family data set
#plotdata_s<-merge(species_catch_pr_cell,expand.grid(x=xvals,y=yvals),all.y=T)
#plotdata_s<-subset(plotdata_s, mean>=0)
#plotdata_s<-subset(plotdata_s, Fam_name!="NO CATCH")

## Create PLOTDATA for GEAR and CPUE
TBplotdata<-merge(TBspecies_catch_pr_cell,expand.grid(x=xvals,y=yvals),all.y=T)
HLplotdata<-merge(HLspecies_catch_pr_cell,expand.grid(x=xvals,y=yvals),all.y=T)
GNplotdata<-merge(GNspecies_catch_pr_cell,expand.grid(x=xvals,y=yvals),all.y=T)
topplotdata<-merge(top_catch_pr_cell,expand.grid(x=xvals,y=yvals),all.y=T)

#select non NA lines
TBplotdata<-subset(TBplotdata, mean>=0)
HLplotdata<-subset(HLplotdata, mean>=0)
GNplotdata<-subset(GNplotdata, mean>=0)
topplotdata<-subset(topplotdata, mean>=0)

#define intervals for legend in plot
il<-c("<0.02", "0.02-0.05", "0.05-0.1","0.1-0.5", "0.5-1", ">1")

#Selecting Sudan from world map
sudanmap<-map_data("worldHires", "Sudan")
#select only E part of country
sudanmap2<-subset(sudanmap, long>36)

#Selecting only coastline to plot as polygon
sudanmap2<-subset(sudanmap, long>=36.5)
sudanmap2<-subset(sudanmap2, long<38.7)
sudanmap2<-subset(sudanmap2, lat>=18)
sudanmap2<-subset(sudanmap2, lat<=22)
dim(sudanmap2)
sudansmall<-sudanmap2[1:4]
top<-c(36.5, 22, 1, 8621)
bottom<-c(36.5, 18, 1, 8622)
sudanmap3<-rbind(sudansmall, top, bottom)

# legends for management areas
mlon<-c(37, 37.4, 37.2, 37.3, 37.5, 37.9, 38.3)
mlat<-c(21.7, 20.92, 20.3, 19.9, 19, 19.3, 18.5)
mname<-c(1, 2, 3, 4, 5, 6, 7)
group<-c(1,1,1,1,1,1,1)
areanum<-as.data.frame(cbind(mlon, mlat, mname, group))
colnames(areanum)<-c("lon", "lat", "mname", "group")

#' MAP TOTAL CATCHES PR CELL (FIGURE 5)
RedSeaMap2<-ggplot(plotdata1) + geom_tile(aes(x,y,fill=factor(ALL_catVar))) + scale_fill_brewer(palette="OrRd", labels=il) + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + theme_classic() + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray90") + xlim(36.5, 39) +ylim(18, 22)  + geom_path( data=ManageAreas.f, aes(x=long, y=lat, group=group) ) + geom_text(data=areanum,  aes(x=lon, y=lat, label=mname)) +xlab("Longitude") +ylab("Latitude")
RedSeaMap2

ggsave("Fig 5 total biomass all gear areas.tiff", width=6.5, height =8, dpi = 400) # save plot to file


#' FACETED MAPS, BY GEAR AND 3 MOST PROMINENT FAMILIES
facets<-c("Lethrinidae", "Lutjanidae", "Serranidae")

#' Add Month. Year survey column as factor
TBplotdata$survey_m<-as.character(TBplotdata$survey)
TBplotdata$survey_m<-replace(TBplotdata$survey_m, grep("2012901", TBplotdata$survey_m), "Nov. 2012")
TBplotdata$survey_m<-replace(TBplotdata$survey_m, grep("2013002", TBplotdata$survey_m), "May 2013")
TBplotdata$survey_m<-replace(TBplotdata$survey_m, grep("2013005", TBplotdata$survey_m), "Nov. 2013")
TBplotdata$survey_f<-factor(TBplotdata$survey_m, levels=c('Nov. 2012','May 2013','Nov. 2013'))


HLplotdata$survey_m<-as.character(HLplotdata$survey)
HLplotdata$survey_m<-replace(HLplotdata$survey_m, grep("2012901", HLplotdata$survey_m), "Nov. 2012")
HLplotdata$survey_m<-replace(HLplotdata$survey_m, grep("2013002", HLplotdata$survey_m), "May 2013")
HLplotdata$survey_m<-replace(HLplotdata$survey_m, grep("2013005", HLplotdata$survey_m), "Nov. 2013")
HLplotdata$survey_f<-factor(HLplotdata$survey_m, levels=c('Nov. 2012','May 2013','Nov. 2013'))


GNplotdata$survey_m<-as.character(GNplotdata$survey)
GNplotdata$survey_m<-replace(GNplotdata$survey_m, grep("2012901", GNplotdata$survey_m), "Nov. 2012")
GNplotdata$survey_m<-replace(GNplotdata$survey_m, grep("2013002", GNplotdata$survey_m), "May 2013")
GNplotdata$survey_m<-replace(GNplotdata$survey_m, grep("2013005", GNplotdata$survey_m), "Nov. 2013")
GNplotdata$survey_f<-factor(GNplotdata$survey_m, levels=c('Nov. 2012','May 2013','Nov. 2013'))



#' TRAPS FACETED MAP (FIGURE SI 1)
TB_map<-ggplot(TBplotdata[TBplotdata$Fam_name2 %in% facets,]) + theme_classic() + geom_tile(aes(x,y,fill=factor(TB_catVar))) + scale_fill_brewer(palette="OrRd", labels=il)  + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + ggtitle("CPUE Traps - by survey") + facet_grid(Fam_name2 ~ survey_f)  + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray90") + geom_tile(data=plotdata1,aes(x=x,y=y),colour="black",fill="white",alpha=0,lwd=0.4) + xlab("Longitude") + ylab("Latitude")
TB_map
ggsave("Fig SI 1 TB_CPUE_by_survey_facets.tiff", scale = 1, width=7.78, height=10, dpi = 400)


#' HANDLINES FACETED MAP (FIGURE SI 2)
HL_map<-ggplot(HLplotdata[HLplotdata$Fam_name2 %in% facets,]) + theme_classic() + geom_tile(aes(x,y,fill=factor(HL_catVar))) + scale_fill_brewer(palette="OrRd", labels=il)  + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + ggtitle("CPUE Handlines - by survey") + facet_grid(Fam_name2 ~ survey_f)  + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray90") + geom_tile(data=plotdata1,aes(x=x,y=y),colour="black",fill="white",alpha=0,lwd=0.4) + xlab("Longitude") + ylab("Latitude")
HL_map
ggsave("Fig SI 2 HL_CPUE_by_survey_facets.tiff", scale = 1, width=7.78, height=10, dpi = 400)


#' HANDLINES FACETED MAP (FIGURE SI 3)
GN_map<-ggplot(GNplotdata[GNplotdata$Fam_name2 %in% facets,]) + theme_classic() + geom_tile(aes(x,y,fill=factor(GN_catVar))) + scale_fill_brewer(palette="OrRd", labels=il)  + labs(x = "LON", y = "LAT", fill = "kg/hr fishing") + ggtitle("CPUE Gillnet - by survey") + facet_grid(Fam_name2 ~ survey_f)  + geom_polygon(data=sudanmap3, aes(x=long, y=lat, group=group, fill=group), fill="gray90") + geom_tile(data=plotdata1,aes(x=x,y=y),colour="black",fill="white",alpha=0,lwd=0.4) + xlab("Longitude") + ylab("Latitude")
GN_map
ggsave("Fig SI 3 GN_CPUE_by_survey_facets.tiff", scale = 1, width=7.78, height=10, dpi = 400)




#' LENGTH WEIGHT ANALYSIS (FIGURE 6)
#' ---------------------------------

study.species<-c("LUTLU06", "LETLE02","LUTLU04","CARSC01","CARSC04","ACAAC28")


lw.select<-data.frame()

for (i in 1:length(study.species)){
  lw.select<-rbind(lw.select, subset(length.weight, species==study.species[i]))
}

#' add latin names
for (i in 1:nrow(lw.select)){
  lw.select[i,16]<-species.list[grep(lw.select[i,3], species.list[,3]),4]
}

cn<-colnames(lw.select)
cn[16]<-c("LatinName")
colnames(lw.select)<-cn

#' add FishBase LW growth columns
for (i in 1:nrow(lw.select)){
  lw.select[i,17]<-species.list[grep(lw.select[i,3], species.list[,3]),5]
  lw.select[i,18]<-species.list[grep(lw.select[i,3], species.list[,3]),6]
  lw.select[i,19]<-species.list[grep(lw.select[i,3], species.list[,3]),7]
  lw.select[i,20]<-species.list[grep(lw.select[i,3], species.list[,3]),8]
  lw.select[i,21]<-species.list[grep(lw.select[i,3], species.list[,3]),9]
  lw.select[i,22]<-species.list[grep(lw.select[i,3], species.list[,3]),10]
}

cn<-colnames(lw.select)
cn[17]<-c("a_FB")
cn[18]<-c("b_FB")
cn[19]<-c("a_N")
cn[20]<-c("b_N")
cn[21]<-c("a_S")
cn[22]<-c("b_S")
colnames(lw.select)<-cn

#' CALCULATE ERRORS BETWEEN OBSERVED WEiGHT AND PREDICTED WEIGHT
#' 

species.lw.table<-data.frame(Species=character(1), N=numeric(1), L_min=numeric(1), L_max=numeric(1), a=numeric(1),a_2_5=numeric(1), a_97=numeric(1), b=numeric(1), b_2_5=numeric(1), b_97=numeric(1), error=numeric(1),  t_test=numeric(1))

for (i in 1:length(study.species)){ 
  weight.table<-cbind(subset(lw.select, species==study.species[i])[,5], subset(lw.select, species==study.species[i])[,6], (species.list[grep(study.species[i], species.list[,3]),5] * subset(lw.select, species==study.species[i])[,5]^species.list[grep(study.species[i], species.list[,3]),6]), (species.list[grep(study.species[i], species.list[,3]),7] * subset(lw.select, species==study.species[i])[,5]^species.list[grep(study.species[i], species.list[,3]),8]), (species.list[grep(study.species[i], species.list[,3]),9] * subset(lw.select, species==study.species[i])[,5]^species.list[grep(study.species[i], species.list[,3]),10]))
  
  weight.table<-as.data.frame(weight.table)  
  
  weight.table[,6]<-(weight.table[,2]-weight.table[,3])/weight.table[,2]  
  colnames(weight.table)<-c("Length", "Obs", "FB", "N", "S", "Err")
  t.fb<-t.test(weight.table[,2], weight.table[,3], paired=TRUE) #test vs FishBase data
  
  test.lw<-subset(lw.select, species==study.species[i])
  nls.lw<-nls(weight~a*length^b, data=test.lw, start = list(a=0.003, b=3), control= list(warnOnly=TRUE))
  #coef(nls.lw)
  
  #' enter species data into species-LW-table
  sp.line<-as.data.frame(list(test.lw[1,3], nrow(test.lw), min(test.lw$length), max(test.lw$length), coef(nls.lw)[1], confint(nls.lw)[1,1], confint(nls.lw)[1,2], coef(nls.lw)[2], confint(nls.lw)[2,1], confint(nls.lw)[2,2], mean(weight.table[,6]), t.fb$p.value))
  colnames(sp.line)<-colnames(species.lw.table)
  species.lw.table<-rbind(species.lw.table, sp.line[1,])
}

species.lw.table<-species.lw.table[2:26,]

#' add latin names
for (i in 1:nrow(species.lw.table)){
  species.lw.table[i,13]<-species.list[grep(species.lw.table[i,1], species.list[,3]),4]
}

cn<-colnames(species.lw.table)
cn[13]<-c("LatinName")
colnames(species.lw.table)<-cn

#' export table
write.csv2(species.lw.table, file= "species_LW_table.csv")

#' L-W PLOTS
lw.colours<- c("gray10", brewer.pal(length(study.species), "Set1"))

for (i in 1:length(study.species)){ 
  
  ss<-subset(lw.select, species==study.species[i])
  lw.plot.1<-ggplot(ss, aes(length, weight)) + geom_point() + ggtitle(species.list[grep(study.species[i], species.list[,3]),4]) + theme_classic() + theme(title = element_text(face="italic")) + xlab("length (cm)") + ylab("weight (g)")
  #' +stat_smooth(colour=lw.colours[1])
  #lw.plot.1
  
  #' adding L-W curve calculated from data
  l.fb<-c(min(subset(lw.select, species==study.species[i])[5]):max(subset(lw.select, species==study.species[i])[5]))
  w.fb<-species.lw.table[grep(study.species[i], species.lw.table[,1]),5] * l.fb^species.lw.table[grep(study.species[i], species.lw.table[,1]),8] 
  w.lo<-species.lw.table[grep(study.species[i], species.lw.table[,1]),6] * l.fb^species.lw.table[grep(study.species[i], species.lw.table[,1]),9]
  w.up<-species.lw.table[grep(study.species[i], species.lw.table[,1]),7] * l.fb^species.lw.table[grep(study.species[i], species.lw.table[,1]),10]
  
  lo.points<-as.data.frame(cbind(l.fb, w.fb, w.lo, w.up))
  
  lw.plot.1 <- lw.plot.1 + geom_line(data=lo.points,  colour=lw.colours[1], aes(l.fb, w.fb))
  
  #' adding local growth curves
  #' Northern Area growth curve (BLUE line)
  l.ln<-c(min(subset(lw.select, species==study.species[i])[5]):max(subset(lw.select, species==study.species[i])[5]))
  w.ln<-species.list[grep(study.species[i], species.list[,3]),7] * l.fb^species.list[grep(study.species[i], species.list[,3]),8] 
  ln.points<-as.data.frame(cbind(l.ln, w.ln))
  
  lw.plot.1 <- lw.plot.1 + geom_line(data=ln.points, colour=lw.colours[3], aes(l.ln, w.ln) )
  
  #' Southern Area growth curve (GREEN line)
  
  l.ls<-c(min(subset(lw.select, species==study.species[i])[5]):max(subset(lw.select, species==study.species[i])[5]))
  w.ls<-species.list[grep(study.species[i], species.list[,3]),9] * l.fb^species.list[grep(study.species[i], species.list[,3]),10] 
  ln.points<-as.data.frame(cbind(l.ls, w.ls))
  
  lw.plot.1<-lw.plot.1 + geom_line(data=ln.points, colour=lw.colours[4], aes(l.ls, w.ls))
  ggsave(paste(study.species[i],".png", sep=""))
  assign(paste(study.species[i],".plot", sep=""), lw.plot.1)
}

#' Combine 6 species plot into one multiplot
lw.layout<-matrix(1:6, nrow=3, byrow=TRUE)
lw.plotlist<-list(LUTLU06.plot, LETLE02.plot, LUTLU04.plot, CARSC01.plot, CARSC04.plot, ACAAC28.plot)

tiff(file="fig 9 LWplot.tiff", width=3000, height=3000, res=400, pointsize=6, compression=c("none"))

multiplot(LUTLU06.plot, LETLE02.plot, LUTLU04.plot, CARSC01.plot, CARSC04.plot, ACAAC28.plot, layout=lw.layout)
dev.off()








###################################
###################################
#' ADDITIONAL ANALYSES
#' --------------------------------
#' SPECIES NUMBERS PR AREA
#' ------------------------------
#' not presented as figures in the MS submitted to Frontiers in Nov'17
ss.list<-unique(catch1$ss)

spue.table<-data.frame(0,0,0,0,0)
colnames(spue.table)<-c("ss","area", "spue", "nspec", "survey")

for (i in 1:length(ss.list)) { 
  spue.table[i,1] <- subset(catch1, catch1$ss == ss.list[i])$ss[1]
  spue.table[i,2] <- subset(catch1, catch1$ss == ss.list[i])$Area[1]
  spue.table[i,3] <- nrow(subset(catch1, catch1$ss == ss.list[i]))/subset(catch1, catch1$ss == ss.list[i])$Fhrs[1] 
  spue.table[i,4] <- nrow(subset(catch1, catch1$ss == ss.list[i])) 
  spue.table[i,5] <- as.integer(substr(ss.list[i], 1,7))
}

spue.agg<-aggregate(spue.table, list(Area = spue.table$area), mean)

sp_n_area <- catch %>% group_by(name) %>% summarise(Tot_N_species=n_distinct(Sci_name))

summary(sp_n_area)

#ANOVA Species Numbers (not corrected for effort)
spue.df<-spue.table
spue.df$area<-factor(spue.df$area, labels = c(1:7))
spue.anova<-lm(nspec ~ area, data = spue.df)
summary(spue.anova)
#significant for area 2 
anova(spue.anova)
confint(spue.anova)
#  not significant

# Residual plot species number
spue.mod <- data.frame(Fitted = fitted(spue.anova), Residuals = resid(spue.anova), Treatment = spue.df$area)

#Tukey post.hoc test
spue.aov<-aov(nspec ~ area, data = spue.df)
posthoc<-TukeyHSD(x=spue.aov, 'area', conf.level=0.95)
# no significan effects

#kruskal Wallis
kruskal.test(nspec ~ area, data = spue.df)
#not significant


#ANOVA Species CPUE
spue.df<-spue.table
spue.df$area<-factor(spue.df$area, labels = c(1:7))

ggplot(spue.df, aes(x = area, y = spue)) +geom_boxplot(fill = "grey80", colour = "blue") +scale_x_discrete() + xlab("Areas") +ylab("CPUEn")

spue.anova<-lm(spue ~ area, data = spue.df)
summary(spue.anova)
#  significant for area 2 and intercept
anova(spue.anova)
confint(spue.anova)
# not significant

#Tukey post.hoc test
spue.aov<-aov(spue ~ area, data = spue.df)
posthoc<-TukeyHSD(x=spue.aov, 'area', conf.level=0.95)
# not significant



#' SPECIES NUMBERS PLOTS
sp.indicies<-by(catch$species, catch$Area, summary)

sp.numbers<-c(0)

species.area.list<-list(1,2,3,4,5,6,7)

for (j in 1:length(species.area.list)) {
  for (i in 1:length(sp.indicies[[j]])){
    if (sp.indicies[[j]][i] != 0) { 
      species.area.list[[j]] <- species.area.list[[j]] +sp.indicies[[j]][i]/sp.indicies[[j]][i] }
    else  { species.area.list[[j]]<-species.area.list[[j]] }
  }
}

m.species<-melt(species.area.list)

#' Species No. Plots
species.plot<-ggplot(m.species, aes(x=L1, y=value)) + geom_bar(stat="identity", fill="steelblue") +theme_bw() + scale_x_reverse(breaks=c(1,2,3,4,5,6,7)) 
species.plot  + coord_flip() + theme(legend.title = element_text(size=14, face="bold")) + theme(legend.text = element_text(size=12)) + ylab("Number of species caught") + xlab("Management areas from N (1) to S (7)") 
ggsave("No species pr area.tiff")



#' SPECIES PR UNIT EFFORT
#' anova analysis
ss.list<-unique(catch1$ss)

spue.table<-data.frame(0,0,0,0,0)
colnames(spue.table)<-c("ss","area", "spue", "nspec", "survey")

for (i in 1:length(ss.list)) { 
  spue.table[i,1] <- subset(catch1, catch1$ss == ss.list[i])$ss[1]
  spue.table[i,2] <- subset(catch1, catch1$ss == ss.list[i])$Area[1]
  spue.table[i,3] <- nrow(subset(catch1, catch1$ss == ss.list[i]))/subset(catch1, catch1$ss == ss.list[i])$Fhrs[1] 
  spue.table[i,4] <- nrow(subset(catch1, catch1$ss == ss.list[i])) 
  spue.table[i,5] <- as.integer(substr(ss.list[i], 1,7))
}

spue.agg<-aggregate(spue.table, list(Area = spue.table$area), mean)

spue.plot<-ggplot(spue.agg, aes(x=area, y=spue)) + geom_bar(stat="identity", fill="steelblue") +theme_bw() + scale_x_reverse(breaks=c(1,2,3,4,5,6,7)) 
spue.plot  + coord_flip() + theme(legend.title = element_text(size=14, face="bold")) + theme(legend.text = element_text(size=12)) + ylab("Species caught pr hour of fishing") + xlab("Management areas from N (upper) to S (lower)") 
ggsave("SPUE species pr area.png")




library(ggplot2)
#library(plyr)
#library(mgcv)
library(dplyr)
library(tidyr)
library(gganimate)
library(scales)
library(tweenr)
library(lubridate)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/UC Davis & Smithsonian/Linnea STRI/R")

###########load data with readr#########
individuals3 <- read.csv("Library/Mobile Documents/com~apple~CloudDocs/UC Davis & STRI/Linnea STRI/R/FFT_Cleaned_Final.csv")
individuals3$timestamp=as.POSIXct((individuals3$timestamp),format="%Y-%m-%d %H:%M:%OS",origin="01-01-1900",tz="UTC")
start1=as.POSIXct("2018-01-20 07:36:00",format="%Y-%m-%d %H:%M:%OS",origin="01-01-1900",tz="America/Panama")
end1=as.POSIXct("2018-01-20 09:30:00",format="%Y-%m-%d %H:%M:%OS",origin="01-01-1900",tz="America/Panama")
start1=with_tz(start1, "UTC")
end1=with_tz(end1, "UTC")
id1=individuals3[which(individuals3$individual.local.identifier=="Bob 4661"),]
id2=individuals3[which(individuals3$individual.local.identifier=="Carlsberg 4673"),]
id1=id1[which(id1$timestamp<end1 & id1$timestamp >= start1),]
id2=id2[which(id2$timestamp<end1 & id2$timestamp >= start1),]


firstpair=rbind(id1,id2)


firstpair$individual.local.identifier=as.factor(firstpair$individual.local.identifier)
firstpair$individual.taxon.canonical.name=as.factor(firstpair$individual.taxon.canonical.name)
library(rgdal)

firstpair2 <- SpatialPointsDataFrame(coords = firstpair[,c(3,4)], data = firstpair,
                                  proj4string=CRS("+proj=longlat +datum=WGS84"))
firstpair2 <- spTransform(firstpair2, CRS("+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
firstpair2=as.data.frame(firstpair2)
firstpair2$timestamp=floor_date(firstpair2$timestamp, "minute")
time=unique(firstpair2$timestamp)
steps=seq(from=1,to=length(time),by=1)
firstpair2$steps=NA
for(i in 1:length(time)){
  firstpair2$steps[which(firstpair2$timestamp==time[i])]=steps[i]
}
#########################code for animating every animal#############################################
####################################################################################################
####################################################################################################



##########format data and smooth interpolation##########
data_edit  <- firstpair2 %>%
  arrange(individual.local.identifier, steps) %>%
  select(steps,timestamp,location.long.2,location.lat.2,individual.local.identifier,individual.taxon.canonical.name) %>%
  rename(x=location.long.2,y=location.lat.2,time=steps,timestamp=timestamp,id=individual.local.identifier,species=individual.taxon.canonical.name) %>%
  mutate(ease="linear")

data_tween <- tween_elements(data_edit, "time", "id", "ease", nframes = 20000) 
colnames(data_tween)=c("time","timestamp","x","y","species", ".frame","id")

data_tween$id=as.factor(data_tween$id)

####load shapefiles for crown#s##
crowns= readOGR(dsn = "~/Library/Mobile Documents/com~apple~CloudDocs/UC Davis & STRI/Linnea STRI/R", layer = "BCI_Dipteryx_Patches")
crowns2 <- spTransform(crowns, CRS("+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))


####load raster of site###
library(raster)  ####note that the raster package interfiers with the tweenr package, detach raster before using tweenr again

map=stack("~/Library/Mobile Documents/com~apple~CloudDocs/UC Davis & STRI/Linnea STRI/R/BCI_WholeIsland_June2015_UAV_Ortho_18cm.tif")
crs(map)="+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
e= extent(data_tween)
e[1]=floor(e[1])-10
e[2]=ceiling(e[2])+10
e[3]=floor(e[3])-10
e[4]=ceiling(e[4])+10
map4=crop(map,e)
#sr <- "+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
crowns2 <-crop(crowns2,e)
crown_df <- fortify(crowns2)

#map4 <- projectRaster(map, crs = sr)

#map4=disaggregate(map,fact=10)
##make ggplot object with raster with crowns####
library(RStoolbox)

theme_set(theme_classic())


island=ggRGB(map4, r = 1, g = 2, b = 3,stretch="lin")+
  theme_bw()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank())

crownmap<- island +
  geom_polygon(data=crown_df,aes(x=long,y=lat,group=group),color="magenta")
  #coord_equal() 

####plot tracks over crown and site layers for annimation###
###Change alpha to change size of the head, wake_length to change length of tail###
data_tween$timestamp=as.character(data_tween$timestamp)
library(ggspatial)
p2 <- crownmap +
  geom_point(data=data_tween,aes(x=x, y=y, color=species),size=2) +
  theme(legend.position = "bottom",legend.box="horizontal")+
  coord_fixed(xlim=c(e[1]-10,e[2]+10), ylim=c(e[3]-10,e[4]+10))+
  annotation_scale(plot_unit = 'm')+ 
  shadow_wake(wake_length = .5,alpha=0.3, wrap=FALSE)+
  transition_time(time) +geom_text(aes(x=min(data_tween$x,na.rm=TRUE),y=max(data_tween$y+40,na.rm=TRUE),label=data_tween$timestamp))+
  ease_aes('linear')


#plot(p2)

library(gapminder)
library(gifski)

#gganimate::animate(p2, nframes = 2000,renderer = gifski_renderer())
gganimate::animate(p2,renderer = gifski_renderer())

anim_save("coati_attraction_gif4.gif")




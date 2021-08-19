
### Set working directory
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/UC Davis & STRI/Linnea STRI/R")

### Load packages
library(ggplot2)
library(plyr)
library(mgcv)
library(dplyr)
library(tidyr)
library(gganimate)
library(scales)
library(tweenr)
library(lubridate)
library(rgdal)
library(readr)
library(sp)
library(raster)

####load raster of site###
library(raster)  ####note that the raster package interfiers with the tweenr package, detach raster before using tweenr again
### Set to plot as one map
#par(mfrow=c(1,1))


### Load the dipteryx layer
crowns <- readOGR(dsn = "~/Library/Mobile Documents/com~apple~CloudDocs/UC Davis & STRI/Linnea STRI/R", layer = "BCI_Dipteryx_Patches")
crowns2 <- spTransform(crowns, CRS("+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(crowns2, col ="magenta", add = TRUE )


### Loop the animals and plot them
library(raster)  ####note that the raster package interfiers with the tweenr package, detach raster before using tweenr again
options(digits=6)

### Load the BCI layer and raster stack
map <- stack("~/Library/Mobile Documents/com~apple~CloudDocs/UC Davis & STRI/Linnea STRI/R/BCI_WholeIsland_June2015_UAV_Ortho_18cm.tif")
crs(map) <- "+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
#map=projectRaster(map, crs = "+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs")
library(ggplot2)
library(RStoolbox)

theme_set(theme_void())

island=ggRGB(map, r = 1, g = 2, b = 3,stretch="lin")+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank())

island_cap2016=island
island_spider2016=island
island_coati2016=island
island_cap2018=island
island_spider2018=island
island_coati2018=island

library("RColorBrewer")

Cap_shape_files2016=list.files("~/Library/Mobile Documents/com~apple~CloudDocs/UC Davis & STRI/Linnea STRI/R/UD/cap2016",pattern=".shp",full.names=TRUE)
#Cap_ud_rasters=list.files("~/Library/Mobile Documents/com~apple~CloudDocs/UC Davis & STRI/Linnea STRI/R/UD/cap",pattern=".tif",full.names=TRUE)

Spider_shape_files2016=list.files("~/Library/Mobile Documents/com~apple~CloudDocs/UC Davis & STRI/Linnea STRI/R/UD/spider2016",pattern=".shp",full.names=TRUE)

#Spider_ud_rasters=list.files("~/Library/Mobile Documents/com~apple~CloudDocs/UC Davis & STRI/Linnea STRI/R/UD/spider",pattern=".tif",full.names=TRUE)
#Spider_shape_files=rev(Spider_shape_files)
#Spider_ud_rasters=rev(Spider_ud_rasters)

Coati_shape_files2016=list.files("~/Library/Mobile Documents/com~apple~CloudDocs/UC Davis & STRI/Linnea STRI/R/UD/coati2016",pattern=".shp",full.names=TRUE)
#Coati_ud_rasters=list.files("~/Library/Mobile Documents/com~apple~CloudDocs/UC Davis & STRI/Linnea STRI/R/UD/coati",pattern=".tif",full.names=TRUE)

Cap_shape_files2018=list.files("~/Library/Mobile Documents/com~apple~CloudDocs/UC Davis & STRI/Linnea STRI/R/UD/cap2018",pattern=".shp",full.names=TRUE)

Spider_shape_files2018=list.files("~/Library/Mobile Documents/com~apple~CloudDocs/UC Davis & STRI/Linnea STRI/R/UD/spider2018",pattern=".shp",full.names=TRUE)

Coati_shape_files2018=list.files("~/Library/Mobile Documents/com~apple~CloudDocs/UC Davis & STRI/Linnea STRI/R/UD/coati2018",pattern=".shp",full.names=TRUE)


### Loop 2016

capblues1 <- c("dodgerblue4", "dodgerblue1", "skyblue3")

library(rgdal)
for(i in 1:length(Cap_shape_files2016)){
  #UD_density=raster(Cap_ud_rasters[[i]])
  #UD_density=projectRaster(UD_density, crs = "+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs")
  UD_outline=readOGR(Cap_shape_files2016[[i]])
  UD_outline <- spTransform(UD_outline, CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs"))

  UD_outline=UD_outline[2,]
  UD_outline=fortify(UD_outline)
  
  #UD_density <- rasterToPoints(UD_density, spatial = TRUE)
  # Then to a 'conventional' dataframe
  #UD_density  <- data.frame(UD_density)
  #colnames(UD_density)[1]=c("UD_density_values")
  #test=(unique(round(UD_density$UD_density_values,digits=2)))
  #UD_density$UD_density_values[which(UD_density$UD_density_values>test[3])]=NA
  island_cap2016<-island_cap2016 + #geom_raster(data = na.omit(UD_density), aes(x,y,fill = UD_density_values))+
    guides(fill = "none") +
    scale_alpha(range = c(.01, 0.5), guide = "none") +
    geom_polygon(data=UD_outline,aes(x=long,y=lat,group=group),color=capblues1[i],fill=NA,size=2)
}

spiderreds1 <- c("red", "indianred2", "firebrick3", "red4")

for(i in 1:length(Spider_shape_files2016)){
  #UD_density=raster(Spider_ud_rasters[[i]])
  #UD_density=projectRaster(UD_density, crs = "+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs")
  UD_outline=readOGR(Spider_shape_files2016[[i]])
  UD_outline <- spTransform(UD_outline, CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs"))
  
  UD_outline=UD_outline[2,]
  UD_outline=fortify(UD_outline)
  
  #UD_density <- rasterToPoints(UD_density, spatial = TRUE)
  # Then to a 'conventional' dataframe
  #UD_density  <- data.frame(UD_density)
  #colnames(UD_density)[1]=c("UD_density_values")
  #test=(unique(round(UD_density$UD_density_values,digits=2)))
  #UD_density$UD_density_values[which(UD_density$UD_density_values>test[3])]=NA
  island_spider2016<-island_spider2016 + #geom_raster(data = na.omit(UD_density), aes(x,y,fill = UD_density_values))+
    guides(fill = "none") +
    scale_alpha(range = c(.01, 0.5), guide = "none") +
    geom_polygon(data=UD_outline,aes(x=long,y=lat,group=group),color=spiderreds1[i],fill=NA,size=2)
}

coatipurples1 <- c("mediumorchid1", "orchid", "mediumorchid4", "mediumpurple2", "mediumpurple4", "purple2", "mediumpurple3")

for(i in 1:length(Coati_shape_files2016)){
  #UD_density=raster(Coati_ud_rasters[[i]])
  #UD_density=projectRaster(UD_density, crs = "+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs")
  UD_outline=readOGR(Coati_shape_files2016[[i]])
  UD_outline <- spTransform(UD_outline, CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs"))
  
  UD_outline=UD_outline[2,]
  UD_outline=fortify(UD_outline)
  
  #UD_density <- rasterToPoints(UD_density, spatial = TRUE)
  # Then to a 'conventional' dataframe
  #UD_density  <- data.frame(UD_density)
  #colnames(UD_density)[1]=c("UD_density_values")
  #test=(unique(round(UD_density$UD_density_values,digits=2)))
  #UD_density$UD_density_values[which(UD_density$UD_density_values>test[3])]=NA
  island_coati2016<-island_coati2016 + #geom_raster(data = na.omit(UD_density), aes(x,y,fill = UD_density_values))+
    guides(fill = "none") +
    scale_alpha(range = c(.01, 0.5), guide = "none") +
    geom_polygon(data=UD_outline,aes(x=long,y=lat,group=group),color=coatipurples1[i],fill=NA, size=2)
}

### Loop 2018
library(rgdal)

capblues2 <- c("dodgerblue4", "dodgerblue1", "skyblue3", "royalblue", "navyblue")

for(i in 1:length(Cap_shape_files2018)){
  #UD_density=raster(Cap_ud_rasters[[i]])
  #UD_density=projectRaster(UD_density, crs = "+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs")
  UD_outline=readOGR(Cap_shape_files2018[[i]])
  UD_outline <- spTransform(UD_outline, CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs"))
  
  UD_outline=UD_outline[2,]
  UD_outline=fortify(UD_outline)
  
  #UD_density <- rasterToPoints(UD_density, spatial = TRUE)
  # Then to a 'conventional' dataframe
  #UD_density  <- data.frame(UD_density)
  #colnames(UD_density)[1]=c("UD_density_values")
  #test=(unique(round(UD_density$UD_density_values,digits=2)))
  #UD_density$UD_density_values[which(UD_density$UD_density_values>test[3])]=NA
  island_cap2018<-island_cap2018 + #geom_raster(data = na.omit(UD_density), aes(x,y,fill = UD_density_values))+
    #scale_fill_distiller(palette="Blues")+  
    guides(fill = "none") +
    scale_alpha(range = c(.01, 0.5), guide = "none") +
    geom_polygon(data=UD_outline,aes(x=long,y=lat,group=group),color=capblues2[i],fill=NA,size=2)
}

spiderreds2 <- c("red", "indianred2", "firebrick3", "red4")
for(i in 1:length(Spider_shape_files2018)){
  #UD_density=raster(Spider_ud_rasters[[i]])
  #UD_density=projectRaster(UD_density, crs = "+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs")
  UD_outline=readOGR(Spider_shape_files2018[[i]])
  UD_outline <- spTransform(UD_outline, CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs"))
  
  UD_outline=UD_outline[2,]
  UD_outline=fortify(UD_outline)
  
  #UD_density <- rasterToPoints(UD_density, spatial = TRUE)
  # Then to a 'conventional' dataframe
  #UD_density  <- data.frame(UD_density)
  #colnames(UD_density)[1]=c("UD_density_values")
  #test=(unique(round(UD_density$UD_density_values,digits=2)))
  #UD_density$UD_density_values[which(UD_density$UD_density_values>test[3])]=NA
  island_spider2018<-island_spider2018 + #geom_raster(data = na.omit(UD_density), aes(x,y,fill = UD_density_values))+
    #scale_fill_distiller(palette="Blues")+  
    guides(fill = "none") +
    scale_alpha(range = c(.01, 0.5), guide = "none") +
    geom_polygon(data=UD_outline,aes(x=long,y=lat,group=group),color=spiderreds2[i],fill=NA,size=2)
}

coatipurples2 <- c("mediumorchid1", "orchid", "mediumorchid4", "mediumpurple2", "mediumpurple4", "purple2", "mediumpurple3", "violet", "plum2")

for(i in 1:length(Coati_shape_files2018)){
  #UD_density=raster(Coati_ud_rasters[[i]])
  #UD_density=projectRaster(UD_density, crs = "+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs")
  UD_outline=readOGR(Coati_shape_files2018[[i]])
  UD_outline <- spTransform(UD_outline, CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs"))
  
  UD_outline=UD_outline[2,]
  UD_outline=fortify(UD_outline)
  
  #UD_density <- rasterToPoints(UD_density, spatial = TRUE)
  # Then to a 'conventional' dataframe
  #UD_density  <- data.frame(UD_density)
  #colnames(UD_density)[1]=c("UD_density_values")
  #test=(unique(round(UD_density$UD_density_values,digits=2)))
  #UD_density$UD_density_values[which(UD_density$UD_density_values>test[3])]=NA
  island_coati2018<-island_coati2018 + #geom_raster(data = na.omit(UD_density), aes(x,y,fill = UD_density_values))+
    #scale_fill_distiller(palette="Purples")+  
    guides(fill = "none") +
    scale_alpha(range = c(.01, 0.5), guide = "none") +
    geom_polygon(data=UD_outline,aes(x=long,y=lat,group=group),color=coatipurples2[i],fill=NA,size=2)
}

crownmap<- island +
  geom_polygon(data=crowns,aes(x=long,y=lat,group=group),color="magenta", fill="magenta")

library(ggpubr)
ggarrange(island_cap2016,
          island_spider2016,
          island_coati2016,
          island_cap2018,
          island_spider2018,
          island_coati2018,
          crownmap,
          common.legend=TRUE,
          legend="bottom")















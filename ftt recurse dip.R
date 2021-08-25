
library(devtools)
devtools::install_github("cbracis/recurse")
library(recurse)
library(scales)
library(sp)
library(getPass)  
library(move)

pass <- getPass::getPass() ##keep password confidential
loginStored <- movebankLogin(username="Shauhin", password=pass)

## read in all gps data for 2018


data2 <- getMovebankData(study=1120749252 ,  login=loginStored)
data2=spTransform(data2, CRS("+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))


library(rgeos)
library(rgdal)

patches=readOGR("C:/Users/salavi/Documents/BCI_Dipteryx_Patches.shp")
patches <- spTransform(patches, CRS("+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

visits=c()
POI=(over(data2, patches))
POI=na.omit(unique(POI$PatchID))

for (i in 1:length(POI)){tryCatch({
  patch=patches[which(patches[,]$PatchID==POI[i]),]
  trueCentroids = gCentroid(patch,byid=TRUE)
  trueCentroids=as.data.frame(trueCentroids)
  revisits=getRecursionsInPolygon(data2,patch,timeunits="mins")
  revisits=as.data.frame(revisits$revisitStats)
  revisits$PatchID=patches[i,]$PatchID[1]
  revisits$Centroid_X=trueCentroids$x
  revisits$Centroid_Y=trueCentroids$y
  visits[i]=list(revisits)
}, error=function(e){cat("ERROR :",conditionMessage(e), "/n")})
}

visits=do.call(rbind,visits)
visits=visits[,-(2:4)]
visits$Species=NA
colnames(visits)[1]="animalID"
for(i in 1:nrow(data2@idData)){
  visits$Species[which(visits$id==as.character(rownames(data2@idData)[i]))]=as.character(data2@idData$taxon_canonical_name[i])

}

visits$entranceTime = lubridate::with_tz(visits$entranceTime, tzone = "America/Panama")
visits$exitTime = lubridate::with_tz(visits$exitTime, tzone = "America/Panama")

visits=visits[-which(visits$Species=="Potos flavus"),]
visits=visits[-which(visits$Species=="Nasua narica"),]
visits$Species[which(visits$Species=="Cebus capucinus")]="Capuchin"
visits$Species[which(visits$Species=="Ateles geoffroyi")]="Spider monkey"
visits=visits[with(visits, order(entranceTime)), ]
visits=visits[,c(1,10,3,4,5,8,9)]
write.csv(visits,file = "MonkeyGPS2.csv")


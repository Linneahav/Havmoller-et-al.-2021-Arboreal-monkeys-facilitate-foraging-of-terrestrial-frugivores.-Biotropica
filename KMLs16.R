

setwd("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/KMLs 2016")


library(getPass)  
library(move)

pass <- getPass::getPass() ##keep password confidential
loginStored <- movebankLogin(username="Shauhin", password=pass)

Veruca <- getMovebankData(study=1120749252 , animalName="Veruca 4690", login=loginStored)
Veruca <- data.frame(Veruca@data)
Veruca$id="Veruca 4690"

# Vielle <- getMovebankData(study=1120749252 , animalName="Vielle 4670", login=loginStored)
# Vielle <- data.frame(Vielle@data)
# Vielle$id="Vielle 4670"

Sofie <- getMovebankData(study=1120749252 , animalName="Sofie 4674", login=loginStored)
Sofie <- data.frame(Sofie@data)
Sofie$id="Sofie 4674"

Pliny <- getMovebankData(study=1120749252 , animalName="Pliny 4675", login=loginStored)
Pliny <- data.frame(Pliny@data)
Pliny$id="Pliny 4675"

Ornette <- getMovebankData(study=1120749252 , animalName="Ornette 4669", login=loginStored)
Ornette <- data.frame(Ornette@data)
Ornette$id="Ornette 4669"

Olga <- getMovebankData(study=1120749252 , animalName="Olga 4657", login=loginStored)
Olga <- data.frame(Olga@data)
Olga$id="Olga 4657"

Mimi <- getMovebankData(study=1120749252 , animalName="Mimi 4660", login=loginStored)
Mimi <- data.frame(Mimi@data)
Mimi$id="Mimi 4660"

Kyle <- getMovebankData(study=1120749252 , animalName="Kyle 4692", login=loginStored)
Kyle <- data.frame(Kyle@data)
Kyle$id="Kyle 4692"

Ibeth <- getMovebankData(study=1120749252 , animalName="Ibeth 4654", login=loginStored)
Ibeth <- data.frame(Ibeth@data)
Ibeth$id="Ibeth 4654"

Greg <- getMovebankData(study=1120749252 , animalName="Greg 4689", login=loginStored)
Greg <- data.frame(Greg@data)
Greg$id="Greg 4689"

# Gillian <- getMovebankData(study=1120749252 , animalName="Gillian 4671", login=loginStored)
# Gillian <- data.frame(Gillian@data)
# Gillian$id="Gillian 4671"

Ellie <- getMovebankData(study=1120749252 , animalName="Ellie 4668", login=loginStored)
Ellie <- data.frame(Ellie@data)
Ellie$id="Ellie 4668"

Clementina <- getMovebankData(study=1120749252 , animalName="Clementina 4672", login=loginStored)
Clementina <- data.frame(Clementina@data)
Clementina$id="Clementina 4672"

Chibi <- getMovebankData(study=1120749252 , animalName="Chibi 4693", login=loginStored)
Chibi <- data.frame(Chibi@data)
Chibi$id="Chibi 4693"

df <- rbind(Veruca, Sofie, Pliny, Ornette, Olga, Mimi, Kyle, Ibeth, Greg, Ellie, Clementina, Chibi)

unique(df$day)

df <- df[,-c(8:11)]
df1 <- SpatialPointsDataFrame(coords = df[,c(16,15)], data = df,
                              proj4string=CRS("+proj=longlat +datum=WGS84"))
df1 <- sp::spTransform(df1, CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
df1=as.data.frame(df1)

colnames(df1)[25] <-"x"
colnames(df1)[26] <-"y"

df1<-df1[!is.na(df1$x), ]

df1$timestamp<-as.POSIXct(df1$timestamp,tz="UTC")
df1$timestamp<-lubridate::with_tz(df1$timestamp, tzone = "America/Panama")

dyadDurat<-read.csv("C:/Users/salavi/Documents/dyadDurat_16.csv")
dyadDurat<-dyadDurat[,-1]
dyadDurat$timestamp<-as.POSIXct(dyadDurat$timestamp,tz="America/Panama") ### recognize time as class time

dyadDurat1 <- dyadDurat
#dyadDurat1 <- dyadDurat[-which(dyadDurat$id1=="Sahti 4693" | dyadDurat$id2=="Sahti 4693" | dyadDurat$id1=="Emma 5762" | dyadDurat$id2=="Emma 5762"), ]

library(spacetime)


species.func<-function(IDvec){
  outputVec<-c()
  for(i in 1:length(IDvec)){
    if(IDvec[i]=="Mimi 4660" ||IDvec[i]=="Ibeth 4654" || IDvec[i]=="Olga 4657"){
      outputVec[i]<-'Capuchin'
    }else{
      if(IDvec[i]=="Pliny 4675" || IDvec[i]=="Ornette 4669" || IDvec[i]=="Sofie 4674" || IDvec[i]=="Ellie 4668" || IDvec[i]=="Clementina 4672"){
        outputVec[i]<-'Coati'
      }else{
        if(IDvec[i]=="Veruca 4690" || IDvec[i]=="Kyle 4692" || IDvec[i]=="Greg 4689" || IDvec[i]=="Chibi 4693"){
          outputVec[i]<-'Spider Monkey'
        }
      }
    }
  }
  return(outputVec)
}
dyadDurat1$species1<-species.func(dyadDurat1$id1)
dyadDurat1$species2<-species.func(dyadDurat1$id2)
dyadDurat1
tail(dyadDurat1)

### Capuchiner and spider monkey
df.one<-dyadDurat1[dyadDurat1$species1=='Capuchin' & dyadDurat1$species2=='Capuchin',]
df.two<-dyadDurat1[dyadDurat1$species1=='Capuchin' & dyadDurat1$species2=='Spider Monkey',]
df.three<-dyadDurat1[dyadDurat1$species1=='Spider Monkey' & dyadDurat1$species2=='Capuchin',]
df.four<-dyadDurat1[dyadDurat1$species1=='Spider Monkey' & dyadDurat1$species2=='Spider Monkey',]

monkey.df<-rbind(df.one,df.two,df.three,df.four)
monkey.df

### Monkey-coati
df.four <- dyadDurat1[dyadDurat1$species1=='Capuchin' & dyadDurat1$species2=='Coati',]
df.five <- dyadDurat1[dyadDurat1$species1=='Coati' & dyadDurat1$species2=='Capuchin',]
df.six <- dyadDurat1[dyadDurat1$species1=='Spider Monkey' & dyadDurat1$species2=='Coati',]
df.seven <- dyadDurat1[dyadDurat1$species1=='Coati' & dyadDurat1$species2=='Spider Monkey',]

mon.coati.df <- rbind(df.two,df.three,df.four,df.five,df.six,df.seven)
nrow(mon.coati.df)

coati.df <- dyadDurat1[dyadDurat1$species1=='Coati' & dyadDurat1$species2=='Coati',]


#### Important!! Set directory!!!
setwd("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/KMLs 2016/Rasmus 2016")

idData <- data.frame(matrix(nrow = nrow(mon.coati.df),ncol = 4))
names(idData)<-c('interaction.id','original.row','id1','id2')


#### Split id1 and id2 so they can have different colours #####

library(plotKML)
library(sp)
counter<-1
for(i in sample(nrow(mon.coati.df))){ ### for each interaction
  start.time<-mon.coati.df$timestamp[i] -60*60## save the time an hour before the interaction begins
  end.time <- mon.coati.df$timestamp[i]+60*mon.coati.df$duration[i] + 60*60 ## save the time an hour after the interaction ends
  new_rowsraw<-which(df1$id==as.character(mon.coati.df$id1[i]) & df1$timestamp >= start.time & df1$timestamp <= end.time | df1$id==as.character(mon.coati.df$id2[i]) & df1$timestamp >= start.time & df1$timestamp <= end.time)  
  tempraw<-df1[new_rowsraw,]
  
  new_rows1<-which(df1$id==as.character(mon.coati.df$id1[i]) & df1$timestamp >= start.time & df1$timestamp <= end.time)  
  
  temp<-df1[new_rows1,]
#  temp<-temp[which(duplicated(temp$timestamp)|duplicated(temp$timestamp,fromLast = T)),] ## Only includes timestamps for which each individual has a data point (important for plotKML function, I think)
  temp<-temp[order(temp$timestamp),]
  times<-temp$timestamp
  coordinates(temp)<-c("x","y")
  proj4string(temp)<-CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  temp_sp<-as(temp,"SpatialPoints")
  temp_ST<- STIDF(temp_sp,times,data=data.frame(time=temp$timestamp))
  counter
  
  plotKML(temp_ST, points_names=c("id1"),colour_scale=c("yellow", "magenta"),
          file.name = paste( counter, gsub( ':', '', gsub( '-', '', gsub( ' ', '_', mon.coati.df$timestamp[i] ) ) ),"1.kml",sep="_"),fixed = TRUE)
  
  new_rows2<-which(df1$id==as.character(mon.coati.df$id2[i]) & df1$timestamp >= start.time & df1$timestamp <= end.time)  
  temp<-df1[new_rows2,]
  #temp<-temp[which(duplicated(temp$timestamp)|duplicated(temp$timestamp,fromLast = T)),] ## Only includes timestamps for which each individual has a data point (important for plotKML function, I think)
  temp<-temp[order(temp$timestamp),]
  times<-temp$timestamp
  coordinates(temp)<-c("x","y")
  proj4string(temp)<-CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  temp_sp<-as(temp,"SpatialPoints")
  temp_ST<- STIDF(temp_sp,times,data=data.frame(time=temp$timestamp))
  counter
  
  plotKML(temp_ST, points_names=c("id2"),colour_scale=c("red", "blue"),
          file.name = paste( counter, gsub( ':', '', gsub( '-', '', gsub( ' ', '_', mon.coati.df$timestamp[i] ) ) ),"2.kml",sep="_"),fixed = TRUE)
  
  
  vec<-c(counter,i,as.character(unique(tempraw$id)))
  idData[counter,]<-vec
  counter<-counter+1
}

write.csv(idData,file= "interaction_identifier 2016.csv")



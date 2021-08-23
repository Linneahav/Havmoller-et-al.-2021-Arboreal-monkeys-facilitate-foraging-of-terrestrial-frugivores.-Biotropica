setwd("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data for Movebank")
library(readr)
devtools::install_github("wrathematics/getPass")

library(getPass)  
library(move)

pass <- getPass::getPass() ##keep password confidential
loginStored <- movebankLogin(username="Shauhin", password=pass)

Veruca <- getMovebankData(study=1120749252 , animalName="Veruca 4690", login=loginStored)
Veruca <- data.frame(Veruca@data)
Veruca$id="Veruca 4690"

# Vielle <- getMovebankData(study=1120749252 , animalName="Vielle 4670", login=loginStored)
# Vielle <- data.frame(Vielle@data)

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


df <- df[,-c(8:10)]
df1<-df[!is.na(df$location_long), ]
df1 <- SpatialPointsDataFrame(coords = df[,c(17,16)], data = df1,
                              proj4string=CRS("+proj=longlat +datum=WGS84"))
df1 <- sp::spTransform(df1, CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
df1=as.data.frame(df1)

colnames(df1)[26] <-"x"
colnames(df1)[27] <-"y"


df1<-df1[!is.na(df1$x), ]
df1$timestamp<-as.POSIXct(df1$timestamp,tz="UTC")
df1$timestamp<-lubridate::with_tz(df1$timestamp, tzone = "America/Panama")

tag_names<-unique(df1[,'id'])
nnMatrix<-matrix(NA,nrow = length(unique(df1[,'id'])),ncol =length(unique(df1[,'id'])))
myArray<-array(NA,c(length(unique(df1[,'id'])),length(unique(df1[,'id'])),length(unique(df1$timestamp))),
               dimnames = list(tag_names,
                               tag_names))

dim(myArray)
for(i in 1:length(unique(df1$timestamp))){
  nnMatrix<-matrix(NA,nrow = length(unique(df1[,'id'])),ncol =length(unique(df1[,'id'])),
                   dimnames = list(tag_names,
                                   tag_names))
  timeDat<-df1[df1$timestamp==sort(unique(df1$timestamp))[i],]
  print(i)
  for(a in 1:(length(tag_names)-1)){
    for(b in (a+1):length(tag_names)){
      idA<-timeDat[timeDat[,'id']==unique(df1[,'id'])[a],]
      idB<-timeDat[timeDat[,'id']==unique(df1[,'id'])[b],]
      if(length(idA)>1){
        idA = idA[1,]
      }
      if(length(idB)>1){
        idB = idB[1,]
      }
      if(nrow(idA)==0 | nrow(idB)==0){
        nnMatrix[a,b]<-NA
      }else{
        nnMatrix[a,b]<-sqrt((idA[,'x']-idB[,'x'])**2 + (idA[,'y']-idB[,'y'])**2)
       #tempDF<-data.frame(id1=unique(df1[,'id'])[a],id2=unique(df1[,'id'])[b],timestamp=unique(df1$timestamp)[i],dyadDist= nnMatrix[a,b])
       #newDF<-rbind(newDF,tempDF)
        }
    }
  }
  myArray[,,i]<-nnMatrix
}

rownames(myArray)<-tag_names
colnames(myArray)<-tag_names
dimnames(myArray)[[3]]<-as.character(sort(unique(df1$timestamp)))

library(plyr)

newDF <- adply(myArray, c(1,2,3), .id = c("id1","id2","timestamp"))
names(newDF)[4]<-"DyadDist"

head(newDF)
tail(newDF)

####dimnames(myArray)[[3]][17000:17287] ?????????????

write.csv(newDF,"C:/Users/salavi/Documents/newDF16.csv")


### Load the saved csv-file
# newDF<-read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Dyadic_dist2016/newDF16.csv")
# newDF <- newDF[,-1]
# newDF$timestamp <- as.POSIXct(newDF$timestamp,origin='1970-01-01', tz ="EST")

dyadDurat<-data.frame(id1=character(),id2=character(),timestamp=character(),duration=character())
#dyadDurat<- dyadDurat[-which(dyadDurat$id1=="Gillian" | dyadDurat$id2=="Gillian" | dyadDurat$id1=="Vielle" | dyadDurat$id2=="Vielle"), ]

head(dyadDurat)

tag_names <- as.character(unique(df1$id))

newDF<-newDF[!is.na(newDF$DyadDist),]
#closeDF<-newDF[newDF$DyadDist<dist_thres,]
#closeDF<-closeDF[closeDF$id1!="Sahti 4693",]
#closeDF<-closeDF[closeDF$id2!="Sahti 4693",]
#closeDF<-closeDF[closeDF$id1!="Emma 5762",]
#closeDF<-closeDF[closeDF$id2!="Emma 5762",]

a=1
b=2
dist_thres = 20
samp_int = 4
min_stop = 1 

for(a in 1:(length(tag_names)-1)){
  for(b in (a+1):(length(tag_names))){
    tempDF <- newDF[which(newDF$id1==tag_names[a] & newDF$id2==tag_names[b] & newDF$DyadDist < dist_thres) , ]
    tempDF[,'timestamp']<-as.POSIXct(x= tempDF[,'timestamp'],format=c("%Y-%m-%d %H:%M:%S"))
    
    diff_min <- diff(tempDF$timestamp)
    
    tsig <- (abs(diff_min) > samp_int)
    dsig <- diff(c(T, tsig, T) )
    startIndex <- which(dsig < 0)
    endIndex <- which(dsig > 0)-1
    dur_interact = tempDF$timestamp[endIndex+1]-tempDF$timestamp[startIndex]
    
    if(length(startIndex)>1){
      
      tempDF2<-data.frame(id1=rep(tag_names[a],each=length(startIndex)),id2=rep(tag_names[b],each=length(startIndex)),timestamp=tempDF$timestamp[startIndex],duration=dur_interact)
      dyadDurat<-rbind(dyadDurat,tempDF2)
    } 
  }  
  
}

#write.csv(newDF,"~/Dropbox/UC Davis & Smithsonian/eObs cleaning/newDF2016.csv")


#dyadDurat<-dyadDurat[-which(dyadDurat$id1=="Sahti 4693"),]
#dyadDurat<-dyadDurat[-which(dyadDurat$id1=="Emma 5762"),]
#hist(as.numeric(dyadDurat$duration),30,ylim=c(0,20))
#which(dyadDurat$duration<0)
dyadDurat1<-dyadDurat
dyadDurat1$duration<-as.numeric(dyadDurat1$duration)
dyadDurat1$duration<-abs(dyadDurat1$duration)
#longDurat<-dyadDurat1[dyadDurat1$duration>4,]
#hist(longDurat$duration,30,freq=F)
#closeDF[closeDF$id1=="Limon 5215" & closeDF$id2=="Peter Nelson 5774",]


#write.csv(tempDF,"/Users/RGH/Desktop/eObs cleaning/tempDF.csv")
#write.csv(newDF,"/Users/RGH/Desktop/eObs cleaning/newDF.csv")
write.csv(dyadDurat1,file="C:/Users/salavi/Documents/dyadDurat_16.csv")
#write.csv(closeDF,"~/Dropbox/UC Davis & Smithsonian/eObs cleaning/dyadic_dist/closeDF150m.csv")

head(dyadDurat1)

newDF<-read.csv("/Users/rhavmoeller/Desktop/Dyaddist/newDF.csv")

head(newDF)


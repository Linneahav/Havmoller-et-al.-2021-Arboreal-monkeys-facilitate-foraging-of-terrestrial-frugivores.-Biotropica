setwd("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018")

devtools::install_github("wrathematics/getPass")

library(getPass)  
library(move)

names=read.csv("Interaction_data_from_KLMs.csv")

pass <- getPass::getPass() ##keep password confidential
loginStored <- movebankLogin(username="Shauhin", password=pass)

Avery <- getMovebankData(study=1120749252 , animalName="Avery 4671", login=loginStored)
Avery <- data.frame(Avery@data)
Avery$id="Avery 4671"

Bob <- getMovebankData(study=1120749252 , animalName="Bob 4661", login=loginStored)
Bob <- data.frame(Bob@data)
Bob$id="Bob 4661"

Da_Vinci <- getMovebankData(study=1120749252 , animalName="Da Vinci 5764", login=loginStored)
Da_Vinci <- data.frame(Da_Vinci@data)
Da_Vinci$id="Da Vinci 5764"

Fonta_Flora <- getMovebankData(study=1120749252 , animalName="Fonta Flora 4689", login=loginStored)
Fonta_Flora <- data.frame(Fonta_Flora@data)
Fonta_Flora$id="Fonta Flora 4689"

Galena <- getMovebankData(study=1120749252 , animalName="Galena 5775", login=loginStored)
Galena <- data.frame(Galena@data)
Galena$id="Galena 5775"

Golliath <- getMovebankData(study=1120749252 , animalName="Golliath 5214", login=loginStored)
Golliath <- data.frame(Golliath@data)
Golliath$id="Golliath 5214"

Limon <- getMovebankData(study=1120749252 , animalName="Limon 5215", login=loginStored)
Limon <- data.frame(Limon@data)
Limon$id="Limon 5215"

Carlsberg <- getMovebankData(study=1120749252 , animalName="Carlsberg 4673", login=loginStored)
Carlsberg <- data.frame(Carlsberg)
Carlsberg <- Carlsberg[,-c(28:30)]
Carlsberg$id="Carlsberg 4673"

Inez <- getMovebankData(study=1120749252 , animalName="Inez 5213", login=loginStored)
Inez <- data.frame(Inez)
Inez <- Inez[,-c(28:30)]
Inez$id="Inez 5213"

Martinelli <- getMovebankData(study=1120749252 , animalName="Martinelli 5763", login=loginStored)
Martinelli <- data.frame(Martinelli)
Martinelli <- Martinelli[,-c(28:30)]
Martinelli$id="Martinelli 5763"

Norah <- getMovebankData(study=1120749252 , animalName="Norah 4655", login=loginStored)
Norah <- data.frame(Norah)
Norah <- Norah[,-c(28:30)]
Norah$id="Norah 4655"

Peter_Nelson <- getMovebankData(study=1120749252 , animalName="Peter Nelson 5774", login=loginStored)
Peter_Nelson <- data.frame(Peter_Nelson)
Peter_Nelson <- Peter_Nelson[,-c(28:30)]
Peter_Nelson$id="Peter Nelson 5774"

Riwaka <- getMovebankData(study=1120749252 , animalName="Riwaka 4669", login=loginStored)
Riwaka <- data.frame(Riwaka)
Riwaka <- Riwaka[,-c(28:30)]
Riwaka$id="Riwaka 4669"

Thelonious <- getMovebankData(study=1120749252 , animalName="Thelonious 4668", login=loginStored)
Thelonious <- data.frame(Thelonious)
Thelonious <- Thelonious[,-c(28:30)]
Thelonious$id="Thelonious 4668"

Valoy <- getMovebankData(study=1120749252 , animalName="Valoy 5766", login=loginStored)
Valoy <- data.frame(Valoy)
Valoy <- Valoy[,-c(28:30)]
Valoy$id="Valoy 5766"

Zola <- getMovebankData(study=1120749252 , animalName="Zola 5212", login=loginStored)
Zola <- data.frame(Zola)
Zola <- Zola[,-c(28:30)]
Zola$id="Zola 5212"

df <- rbind(Avery, Bob, Da_Vinci, Galena, Golliath, Fonta_Flora, Limon, Carlsberg, Inez, Martinelli, Norah, Peter_Nelson, Thelonious, Riwaka, Valoy, Zola)
df=data.frame(df)


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
  timeDat<-df1[which(df1$timestamp==sort(unique(df1$timestamp))[i]),]
  print(i)
  for(a in 1:(length(tag_names)-1)){
    for(b in (a+1):length(tag_names)){
      idA<-timeDat[which(timeDat[,'id']==unique(df1[,'id'])[a]),]
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

write.csv(newDF,"C:/Users/salavi/Documents/newDF18.csv")


### Load the saved csv-file
# newDF<-read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Dyadic_dist2018/newDF18.csv")
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
    
    diff_min <- as.numeric(diff(tempDF$timestamp))
    
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

#write.csv(newDF,"~/Dropbox/UC Davis & Smithsonian/eObs cleaning/newDF2018.csv")


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
write.csv(dyadDurat1,file="C:/Users/salavi/Documents/dyadDurat_18.csv")
#write.csv(closeDF,"~/Dropbox/UC Davis & Smithsonian/eObs cleaning/dyadic_dist/closeDF150m.csv")

head(dyadDurat1)

newDF<-read.csv("/Users/rhavmoeller/Desktop/Dyaddist/newDF.csv")

head(newDF)





devtools::install_github("wrathematics/getPass")

library(getPass)  
library(move)

pass <- getPass::getPass() ##keep password confidential
loginStored <- movebankLogin(username="Shauhin", password=pass)

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

Avery <- getMovebankData(study=1120749252 , animalName="Avery 4671", login=loginStored)
Avery <- data.frame(Avery@data)
Avery$id="Avery 4671"

Carlsberg <- getMovebankData(study=1120749252 , animalName="Carlsberg 4673", login=loginStored)
Carlsberg <- data.frame(Carlsberg)
Carlsberg <- Carlsberg[,-c(28:30)]
Carlsberg$id="Carlsberg 4673"

Emma <- getMovebankData(study=1120749252 , animalName="Emma 5762", login=loginStored)
Emma <- data.frame(Emma)
Emma <- Emma[,-c(28:30)]
Emma$id="Emma 5762"

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

# Riwaka <- getMovebankData(study=1120749252 , animalName="Riwaka 4669", login=loginStored)
# Riwaka <- data.frame(Riwaka)
# Riwaka$id="Riwaka 4669"

Thelonious <- getMovebankData(study=1120749252 , animalName="Thelonious 4668", login=loginStored)
Thelonious <- data.frame(Thelonious)
Thelonious <- Thelonious[,-c(28:30)]
Thelonious$id="Thelonious 4668"

Sahti <- getMovebankData(study=1120749252 , animalName="Sahti 4693", login=loginStored)
Sahti <- data.frame(Sahti)
Sahti <- Sahti[,-c(28:30)]
Sahti$id="Sahti 4693"

Valoy <- getMovebankData(study=1120749252 , animalName="Valoy 5766", login=loginStored)
Valoy <- data.frame(Valoy)
Valoy <- Valoy[,-c(28:30)]
Valoy$id="Valoy 5766"

Zola <- getMovebankData(study=1120749252 , animalName="Zola 5212", login=loginStored)
Zola <- data.frame(Zola)
Zola <- Zola[,-c(28:30)]
Zola$id="Zola 5212"

df <- rbind(Bob, Da_Vinci, Galena, Golliath, Fonta_Flora, Limon, Avery, Carlsberg, Emma, Inez, Martinelli, Norah, Peter_Nelson, Thelonious, Sahti, Valoy, Zola)

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

dyadDurat<-read.csv("C:/Users/salavi/Documents/dyadDurat_18.csv")
dyadDurat<-dyadDurat[,-1]
dyadDurat$timestamp<-as.POSIXct(dyadDurat$timestamp,tz="America/Panama") ### recognize time as class time

dyadDurat1 <- dyadDurat
#dyadDurat1 <- dyadDurat[-which(dyadDurat$id1=="Sahti 4693" | dyadDurat$id2=="Sahti 4693" | dyadDurat$id1=="Emma 5762" | dyadDurat$id2=="Emma 5762"), ]

library(spacetime)

species.func<-function(IDvec){
  outputVec<-c()
  for(i in 1:length(IDvec)){
    if(IDvec[i]=="Bob 4661" ||IDvec[i]=="Da Vinci 5764" || IDvec[i]=="Martinelli 5763" || IDvec[i]=="Norah 4655" || IDvec[i]=="Valoy 5766"){
      outputVec[i]<-'Capuchin'
    }else{
      if(IDvec[i]=="Fonta Flora 4689" || IDvec[i]=="Galena 5775" || IDvec[i]=="Avery 4671" || IDvec[i]=="Carlsberg 4673" || IDvec[i]=="Peter Nelson 5774" || IDvec[i]=="Thelonious 4668" || IDvec[i]=="Golliath 5214" || IDvec[i]=="Riwaka 4669"){
        outputVec[i]<-'Coati'
      }else{
        if(IDvec[i]=="Limon 5215" || IDvec[i]=="Inez 5213" || IDvec[i]=="Zola 5212"){
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

### Capuchin and spider monkey interactions
df.one<-dyadDurat1[dyadDurat1$species1=='Capuchin' & dyadDurat1$species2=='Capuchin',]
df.two<-dyadDurat1[dyadDurat1$species1=='Capuchin' & dyadDurat1$species2=='Spider Monkey',]
df.three<-dyadDurat1[dyadDurat1$species1=='Spider Monkey' & dyadDurat1$species2=='Capuchin',]
df.four<-dyadDurat1[dyadDurat1$species1=='Spider Monkey' & dyadDurat1$species2=='Spider Monkey',]

monkey.df<-rbind(df.one,df.two,df.three,df.four)
monkey.df

### Monkey-coati interactions
df.four <- dyadDurat1[dyadDurat1$species1=='Capuchin' & dyadDurat1$species2=='Coati',]
df.five <- dyadDurat1[dyadDurat1$species1=='Coati' & dyadDurat1$species2=='Capuchin',]
df.six <- dyadDurat1[dyadDurat1$species1=='Spider Monkey' & dyadDurat1$species2=='Coati',]
df.seven <- dyadDurat1[dyadDurat1$species1=='Coati' & dyadDurat1$species2=='Spider Monkey',]

mon.coati.df <- rbind(df.two,df.three,df.four, df.five,df.six,df.seven)

### Coati-coati interactions
coati.df <- dyadDurat1[dyadDurat1$species1=='Coati' & dyadDurat1$species2=='Coati',]


#### Split id1 and id2 so they can have different colours

### Make sure to set correct directory, all kml-files will be stored here!
setwd("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/KMLs2018/Rasmus 2018")

idData <- data.frame(matrix(nrow = nrow(mon.coati.df),ncol = 4))
names(idData)<-c('interaction.id','original.row','id1','id2')

library(plotKML)
library(sp)
# Creating kml's
counter<-1
for(i in sample(nrow(mon.coati.df))){ ### for each interaction
  start.time<-mon.coati.df$timestamp[i] -60*60## save the time an hour before the interaction begins
  end.time <- mon.coati.df$timestamp[i]+60*mon.coati.df$duration[i] + 60*60 ## save the time an hour after the interaction ends
  new_rowsraw<-which(df1$id==as.character(mon.coati.df$id1[i]) & df1$timestamp >= start.time & df1$timestamp <= end.time | df1$id==as.character(mon.coati.df$id2[i]) & df1$timestamp >= start.time & df1$timestamp <= end.time)  
  tempraw<-df1[new_rowsraw,]
  
  new_rows1<-which(df1$id==as.character(mon.coati.df$id1[i]) & df1$timestamp >= start.time & df1$timestamp <= end.time)  
  
  temp<-df1[new_rows1,]
  #temp<-temp[which(duplicated(temp$timestamp)|duplicated(temp$timestamp,fromLast = T)),] ## Only includes timestamps for which each individual has a data point (important for plotKML function, I think)
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

# Print csv-file with names of interactions for verification
write.csv(idData,file= "interaction_identifier_2018.csv")



########## Carters original script before Roy changed into id1 and id2 as seperate kml's ###########

setwd("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/KMLs2018/new_coati_coati")

idData <- data.frame(matrix(nrow = nrow(coati.df),ncol = 4))
names(idData)<-c('interaction.id','original.row','id1','id2')

library(plotKML)
library(sp)
counter<-1
for(i in sample(nrow(coati.df))){ ### for each interaction
  start.time<-coati.df$timestamp[i] -60*60## save the time an hour before the interaction begins
  end.time <- coati.df$timestamp[i]+60*coati.df$duration[i] + 60*60 ## save the time an hour after the interaction ends
  new_rows<-which(df1$id==as.character(coati.df$id1[i]) & df1$timestamp >= start.time & df1$timestamp <= end.time | df1$id==as.character(coati.df$id2[i]) & df1$timestamp >= start.time & df1$timestamp <= end.time)  
  temp<-df1[new_rows,]
  temp<-temp[which(duplicated(temp$timestamp)|duplicated(temp$timestamp,fromLast = T)),] ## Only includes timestamps for which each individual has a data point (important for plotKML function, I think)
  temp<-temp[order(temp$timestamp),]
  times<-temp$timestamp
  temp_sp <- SpatialPointsDataFrame(coords = temp[c("x","y")], data = temp,
                                proj4string=CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  #coordinates(temp)<-c("x","y")
  #proj4string(temp)<-CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  #temp_sp<-as(temp,"SpatialPoints")
  temp_ST<- STIDF(temp_sp,times,data=data.frame(time=temp$timestamp))
  counter
  
  plotKML(temp_ST, points_names=c("id1","id2"),colour_scale=rep("#FFFF00", 2),
          file.name=paste(counter,coati.df$timestamp[i],".kml",sep="_"),fixed = TRUE)
  vec<-c(counter,i,as.character(unique(temp$id)))
  idData[counter,]<-vec
  counter<-counter+1
}

write.csv(idData,file= "interaction_identifier.csv")



setwd("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/KMLs2018/new_monkey_monkey")

idData <- data.frame(matrix(nrow = nrow(monkey.df),ncol = 4))
names(idData)<-c('interaction.id','original.row','id1','id2')

library(plotKML)
library(sp)
counter<-1
for(i in sample(nrow(monkey.df))){ ### for each interaction
  start.time<-monkey.df$timestamp[i] -60*60## save the time an hour before the interaction begins
  end.time <- monkey.df$timestamp[i]+60*monkey.df$duration[i] + 60*60 ## save the time an hour after the interaction ends
  new_rows<-which(df1$id==as.character(monkey.df$id1[i]) & df1$timestamp >= start.time & df1$timestamp <= end.time | df1$id==as.character(monkey.df$id2[i]) & df1$timestamp >= start.time & df1$timestamp <= end.time)  
  temp<-df1[new_rows,]
  temp<-temp[which(duplicated(temp$timestamp)|duplicated(temp$timestamp,fromLast = T)),] ## Only includes timestamps for which each individual has a data point (important for plotKML function, I think)
  temp<-temp[order(temp$timestamp),]
  times<-temp$timestamp
  coordinates(temp)<-c("x","y")
  proj4string(temp)<-CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  temp_sp<-as(temp,"SpatialPoints")
  temp_ST<- STIDF(temp_sp,times,data=data.frame(time=temp$timestamp))
  counter
  
  plotKML(temp_ST, points_names=c("id1","id2"),colour_scale=rep("#FFFF00", 2),
          file.name=paste(counter,monkey.df$timestamp[i],".kml",sep="_"),fixed = TRUE)
  vec<-c(counter,i,as.character(unique(temp$id)))
  idData[counter,]<-vec
  counter<-counter+1
}

write.csv(idData,file= "interaction_identifier.csv")

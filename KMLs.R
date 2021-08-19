

setwd("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/KMLs2018")

Bob <- read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018/Bob 4661.csv")
Da_Vinci <- read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018/Da Vinci 5764.csv")
Fonta_Flora <- read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018/Fonta Flora 4689.csv")
Galena <- read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018/Galena 5775.csv")
Golliath <- read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018/Golliath 5214.csv")
Limon <- read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018/Limon 5215.csv")
Avery <- read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018/Avery 4671.csv")
Carlsberg <- read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018/Carlsberg 4673.csv")
Emma <- read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018/Emma 5762.csv")
Inez <- read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018/Inez 5213.csv")
Martinelli <- read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018/Martinelli 5763.csv")
Norah <- read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018/Norah 4655.csv")
Peter_Nelson <- read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018/Peter Nelson 5774.csv")
#Riwaka <- read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018/Riwaka 4669.csv")
Thelonious <- read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018/Thelonious 4668.csv")
Sahti <- read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018/Sahti 4693.csv")
Valoy <- read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018/Valoy 5766.csv")
Zola <- read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Clean data 2018/Zola 5212.csv")

df <- rbind(Bob, Da_Vinci, Galena, Golliath, Fonta_Flora, Limon, Avery, Carlsberg, Emma, Inez, Martinelli, Norah, Peter_Nelson, Thelonious, Sahti, Valoy, Zola)

df <- df[,-c(8:11)]

df1<-df[!is.na(df$x), ]

df1$timestamp<-as.POSIXct(df1$timestamp,tz="EST")

dyadDurat<-read.csv("~/Dropbox/UC Davis & Smithsonian/eObs cleaning/Dyadic_dist/unique_2018_50m_dip20_patches.csv")
dyadDurat<-dyadDurat[,-1]
dyadDurat$timestamp<-as.POSIXct(dyadDurat$timestamp,tz="EST") ### recognize time as class time

dyadDurat1 <- dyadDurat
#dyadDurat1 <- dyadDurat[-which(dyadDurat$id1=="Sahti 4693" | dyadDurat$id2=="Sahti 4693" | dyadDurat$id1=="Emma 5762" | dyadDurat$id2=="Emma 5762"), ]

library(spacetime)

##### Not sure if necessary here, already done in the MonkeyCoati_interac_diff_dist.R script #######
species.func<-function(IDvec){
  outputVec<-c()
  for(i in 1:length(IDvec)){
    if(IDvec[i]=="Bob 4661" ||IDvec[i]=="Da Vinci 5764" || IDvec[i]=="Martinelli 5763" || IDvec[i]=="Norah 4655" || IDvec[i]=="Valoy 5766"){
      outputVec[i]<-'Capuchin'
    }else{
      if(IDvec[i]=="Fonta Flora 4689" || IDvec[i]=="Galena 5775" || IDvec[i]=="Avery 4671" || IDvec[i]=="Carlsberg 4673" || IDvec[i]=="Peter Nelson 5774" || IDvec[i]=="Thelonious 4668" || IDvec[i]=="Golliath 5214"){
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
#df.one<-dyadDurat1[dyadDurat1$species1=='Capuchin' & dyadDurat1$species2=='Capuchin',]
df.two<-dyadDurat1[dyadDurat1$species1=='Capuchin' & dyadDurat1$species2=='Spider Monkey',]
df.three<-dyadDurat1[dyadDurat1$species1=='Spider Monkey' & dyadDurat1$species2=='Capuchin',]
#df.four<-dyadDurat1[dyadDurat1$species1=='Spider Monkey' & dyadDurat1$species2=='Spider Monkey',]

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
          file.name=paste(counter,mon.coati.df$timestamp[i],"1.kml",sep="_"),fixed = TRUE)
  
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
          file.name=paste(counter,mon.coati.df$timestamp[i],"2.kml",sep="_"),fixed = TRUE)
  
  
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
  coordinates(temp)<-c("x","y")
  proj4string(temp)<-CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  temp_sp<-as(temp,"SpatialPoints")
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

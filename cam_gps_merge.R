

## set working directory
setwd("/Users/carterloftus/Documents/Waiting_times_Linnea/")

#load libraries
library(stringr)
library(plyr)
library(hms)
library(getPass)  
library(move)

pass <- getPass::getPass() ##keep password confidential
loginStored <- movebankLogin(username="Shauhin", password=pass)

## read in all gps data for 2018

data <- getMovebankData(study=1120749252 ,  login=loginStored)
whole_df <- data@data
whole_df$id=as.character(data@trackId)
whole_df$individual.taxon.canonical.name=NA
for(i in 1:nrow(data@idData)){
  whole_df$individual.taxon.canonical.name[which(whole_df$id==as.character(rownames(data@idData)[i]))]=as.character(data@idData$taxon_canonical_name[i])

}
whole_df$individual.taxon.canonical.name=trimws(whole_df$individual.taxon.canonical.name)
whole_df$id=trimws(whole_df$id)

whole_df <- whole_df[,-c(8:10)]

whole_df<-whole_df[!is.na(whole_df$location_long), ]
whole_df <- SpatialPointsDataFrame(coords = whole_df[,c(17,16)], data = whole_df,
                              proj4string=CRS("+proj=longlat +datum=WGS84"))
whole_df <- sp::spTransform(whole_df, CRS("+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
whole_df=as.data.frame(whole_df)

colnames(whole_df)[27] <-"x"
colnames(whole_df)[28] <-"y"


whole_df<-whole_df[!is.na(whole_df$x), ]

whole_df_split=split(whole_df,as.factor(whole_df$individual.taxon.canonical.name))
Potos = whole_df_split$`Potos flavus`
Other = rbind(whole_df_split$`Ateles geoffroyi`,whole_df_split$`Cebus capucinus`,whole_df_split$`Nasua narica`)
Potos_time_Y1=seq(from=as.POSIXct("2015-12-14 18:00:00", tz = 'America/Panama'), to=as.POSIXct("2016-04-18 19:00:00", tz = 'America/Panama'), by="1 sec")
Potos_time_Y1=lubridate::with_tz(Potos_time_Y1, tzone = "UTC")
Potos_time_Y2=seq(from=as.POSIXct("2017-12-14 18:00:00", tz = 'America/Panama'), to=as.POSIXct("2018-03-04 19:00:00", tz = 'America/Panama'), by="1 sec")
Potos_time_Y2=lubridate::with_tz(Potos_time_Y2, tzone = "UTC")
Potos_time_Y1=Potos_time_Y1[Potos_time_Y1 %in% Potos$timestamp]
Potos_time_Y2=Potos_time_Y2[Potos_time_Y2 %in% Potos$timestamp]


Other_time_Y1=seq(from=as.POSIXct("2015-12-11 5:00:00", tz = 'America/Panama'), to=as.POSIXct("2016-04-21 19:00:00", tz = 'America/Panama'), by="1 sec")
Other_time_Y1=lubridate::with_tz(Other_time_Y1, tzone = "UTC")
Other_time_Y2=seq(from=as.POSIXct("2017-12-01 5:00:00", tz = 'America/Panama'), to=as.POSIXct("2018-06-14 19:00:00", tz = 'America/Panama'), by="1 sec")
Other_time_Y2=lubridate::with_tz(Other_time_Y2, tzone = "UTC")
Other_time_Y1=Other_time_Y1[Other_time_Y1 %in% Other$timestamp]
Other_time_Y2=Other_time_Y2[Other_time_Y2 %in% Other$timestamp]
Potos_time2_y1=data.frame(cbind(as.character(Potos_time_Y1),as.numeric(as.factor(as.character(cut(Potos_time_Y1,  breaks="24 hours"))))))
Other_time2_y1=data.frame(cbind(as.character(Other_time_Y1),as.numeric(as.factor(as.character(cut(Other_time_Y1,  breaks="24 hours"))))))
Potos_time2_y2=data.frame(cbind(as.character(Potos_time_Y2),as.numeric(as.factor(as.character(cut(Potos_time_Y2,  breaks="24 hours"))))))
Other_time2_y2=data.frame(cbind(as.character(Other_time_Y2),as.numeric(as.factor(as.character(cut(Other_time_Y2,  breaks="24 hours"))))))
colnames(Potos_time2_y1)=c("timestamp","day")
colnames(Potos_time2_y2)=c("timestamp","day")
colnames(Other_time2_y1)=c("timestamp","day")
colnames(Other_time2_y2)=c("timestamp","day")
Potos_time2=rbind(Potos_time2_y1,Potos_time2_y2)
Other_time2=rbind(Other_time2_y1,Other_time2_y2)

Potos_time2$timestamp<- as.POSIXct(Potos_time2$timestamp, tz = 'UTC')
Other_time2$timestamp<- as.POSIXct(Other_time2$timestamp, tz = 'UTC')
Potos$day=NA
Other$day=NA

for(i in 1:nrow(Potos_time2)){
  Potos$day[which(Potos$timestamp==Potos_time2$timestamp[i])]=Potos_time2$day[i]
}
for(i in 1:nrow(Other_time2)){
  Other$day[which(Other$timestamp==Other_time2$timestamp[i])]=Other_time2$day[i]
}

whole_df=rbind(Potos,Other)

## make the timestamps POSIX class
whole_df$fix_timestamp <- lubridate::with_tz(whole_df$timestamp, tzone = "America/Panama")


## create a new column that will store the time (in seconds) since the previous fix
whole_df$time_since_fix <- NA

sum(is.na(whole_df$x)) ## shows that there is no missing location data
sum(is.na(whole_df$day)) 

## save a vector of the ids of the animals
ids <- as.character(unique(whole_df$id))

## fills in the 'time since fix' column, going animal by animal, and day by day
for(id in ids){
  idDat <- whole_df[whole_df$id == id,]
  days <- unique(idDat$day)
  for(d in days){
    dayDat <- idDat[idDat$day == d,]
    dayDat$time_since_fix <- c(NA, as.numeric(diff(dayDat$fix_timestamp),unit = 'secs'))
    idDat[idDat$day == d,] <- dayDat
  }
  whole_df[whole_df$id == id,] <- idDat
}

head(whole_df)
### Finished with setup of GPS data

## read in the meta data that records when the cameras are set up and taken down. We won't process any photos that happen before or after these times, respectively
cam_meta <- read.csv("CameraSetup.csv")
colnames(cam_meta) <-c("startDate","startTime", "stopData", "stopTime", "PatchID")

## create POSIX class timestamps
cam_meta$start_timestamp <- paste(cam_meta$startDate, cam_meta$startTime)
cam_meta$start_timestamp <- as.POSIXct(cam_meta$start_timestamp, tz = 'America/Panama')
cam_meta$stop_timestamp <- paste(cam_meta$stopData, cam_meta$stopTime)
cam_meta$stop_timestamp <- as.POSIXct(cam_meta$stop_timestamp, tz = 'America/Panama')
### Finsished with setup of meta data

## read in the data from the camera traps
files <- list.files(path = "C:/Users/salavi/Downloads/Waiting_times_Linnea/Waiting_times_Linnea/CameraTrapCSV/", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
df_cam <- ldply(as.list(files), read.csv)

## take off the white space of either end of the species names. Replace white space in the middle with a '.'
df_cam$Species <- as.character(df_cam$Species)
df_cam$Species <- trimws(df_cam$Species)
df_cam$Species <- gsub(" ", ".", df_cam$Species)

## change 'Collared.individual' to 'Coati.collared'
df_cam$Species[df_cam$Species == 'Collared.individual'] <- 'Coati.collared'

## give NAs to the observations when there is no animal in the frame
df_cam$Species[df_cam$Species == ''] <- NA

## add '_cam' to the species name. Useful for later 
df_cam$Species <- paste(df_cam$Species,'_','cam',sep='')

## make a POSIX class timestamp
df_cam$timestamp <- paste(df_cam$Date,df_cam$Time,sep = ' ')
df_cam$timestamp <- as.POSIXct(df_cam$timestamp,tz='America/Panama')

## take the maximum number of a particular species in the camera frame at once in a given second, and assign that maximum count to the count for that second (want to eliminate duplicate timestamps that occur because the cameras took more than one photo per second often)
df_cam <- aggregate(df_cam$Counts, by = list(df_cam$timestamp,df_cam$Species,df_cam$PatchID), FUN = max)
names(df_cam) <- c('timestamp', 'Species', 'PatchID', 'Counts')

## shows that there are no more duplicates
dup.counts <- df_cam[duplicated(df_cam[,c('timestamp','Species','PatchID')]) | duplicated(df_cam[,c('timestamp','Species','PatchID')],fromLast = T) ,'Counts']
### Finished with setup of camera data

## read in data of monkeys entering/exiting trees from Shauhin and recurse package
df_gps <- read.csv("MonkeyGPS2.csv")
colnames(df_gps)=c("animalID", "Species", "entranceTime", "exitTime", "PatchID", "Centroid_X", "Centroid_Y")
## make timestamp POSIX element
df_gps$entranceTime <- as.POSIXct(df_gps$entranceTime,  tz='America/Panama')
df_gps$exitTime <- as.POSIXct(df_gps$exitTime ,tz='America/Panama')

## trim white space, add '_gps' to species names
df_gps$Species <- trimws(df_gps$Species)
df_gps$Species <- gsub(" ", ".", df_gps$Species)
df_gps$Species <- paste(df_gps$Species,'_','gps',sep='')
head(df_gps)

## make column for time since the previous fix before the monkey entered the tree and a column for the time until the fix following the monkey leaving the tree. Helps quantify the uncertainty of when the monkey actually entered and exited the tree
df_gps$time_since_fix_ent <- NA
df_gps$time_till_fix_ext <- NA

## This loops fills in the columns created directly above, using the complete gps data loaded it at the very top. It also overwrites the entrance and exit times to be real fix times, not times interpolated by the recurse packaage
for(i in 1:nrow(df_gps)){
  ent.time <- df_gps$entranceTime[i]
  ext.time <- df_gps$exitTime[i]
  name <- df_gps$animalID[i]
  prev.times <- whole_df[whole_df$id == name & whole_df$timestamp < ent.time,'fix_timestamp']
  post.times <- whole_df[whole_df$id == name & whole_df$timestamp >= ent.time,'fix_timestamp']
  df_gps$entranceTime[i] <- post.times[1]
  df_gps$time_since_fix_ent[i] <- as.numeric(post.times[1] - prev.times[length(prev.times)], units = 'secs')
  prev.times <- whole_df[whole_df$id == name & whole_df$timestamp <= ext.time,'fix_timestamp']
  post.times <- whole_df[whole_df$id == name & whole_df$timestamp > ext.time,'fix_timestamp']
  df_gps$exitTime[i] <- prev.times[length(prev.times)]
  df_gps$time_till_fix_ext[i] <- as.numeric(post.times[1] - prev.times[length(prev.times)], units = 'secs')
}

## just a sanity check. Should be 0
sum(df_gps$entranceTime > df_gps$exitTime)

## saves the species names of the monkeys that entered/exited trees, and of animals caught in the camera traps
gps_species <- as.character(unique(df_gps$Species))
cam_species <- as.character(unique(df_cam$Species))

## splits the camera trap data by tree
df_cam_list <- split(df_cam, df_cam$PatchID)

## This is the workhorse of this part of the code. Makes a csv for each tree that is represented in the camera trap data. These csv's have a row for every second that the camera was in action for and says how many of each species (both from GPS and camera trap data) are in the tree at that time
for(dat in df_cam_list){ # for every tree...
  
  sub_df_cam <- dat # just renaming. No purpose to this
  
  # save the tree name
  patch_name <- as.character(unique(sub_df_cam$PatchID)) 
  
  # take the camera start time and stop time from the meta data file
  cam_start <- cam_meta[cam_meta$PatchID == patch_name,'start_timestamp']
  cam_stop <- cam_meta[cam_meta$PatchID == patch_name,'stop_timestamp']
  
  # makes a dataframe with a row for a second and column for every species
  mat <- matrix(0, ncol=length(c(gps_species,cam_species))+2, nrow = length(cam_start:cam_stop))
  temp_df <- as.data.frame(mat)
  names(temp_df) <- c('patch_ID','timestamp', gps_species, cam_species)
  
  # fills in the column declaring the tree name
  temp_df$patch_ID <- patch_name
  
  # fills in the timestamp column, makes it POSIX class
  temp_df$timestamp <- cam_start:cam_stop
  temp_df$timestamp <- as.POSIXct(temp_df$timestamp, origin = '1970-01-01 00:00:00', tz = 'EST')
  
  # subsets the monkey enter/exit GPS data to this tree
  sub_df_gps <- df_gps[df_gps$PatchID == patch_name,]
  
  ## fills in the columns for the species from the camera trap data
  for(specie in cam_species){ # for each species in the camera traps...
    # subset the camera trap data to this species and the times between when the camera was put up and taken down
    n_sub_df_cam <- sub_df_cam[sub_df_cam$Species == specie & sub_df_cam$timestamp <= cam_stop & sub_df_cam$timestamp >= cam_start,] 
    
    # match the timestamps to those in the timestamps in our new dataframe
    inds <- match(n_sub_df_cam$timestamp,temp_df$timestamp)
    
    # fill in the the number of animals of this species in the camera at these seconds matched right above
    temp_df[inds,specie] <- n_sub_df_cam$Counts
  }
  
  
  ## fills in the columns for the species from the monkey enter/exit gps data
  for(g_specie in gps_species){ # for each monkey species
    
    # subset the gps data for this tree to this species
    species_temp <- sub_df_gps[sub_df_gps$Species == g_specie,]
    
    if(nrow(species_temp)!=0){ # only continue if there is actually an occurance of this species in this tree
      
      # make a vector containing the names of the individuals of this species that were in this tree
      id_names <- as.character(unique(species_temp$animalID))
      
      ## for each individual, add one count to the column for their species at the times they were in the tree
      for(id in id_names){ # for each individual...
        
        # subset the GPS data to just that individual's data
        id_temp <- species_temp[species_temp$animalID == id, ]
        
        # make the timestamps POSIX class
        id_temp$entranceTime <- as.POSIXct(id_temp$entranceTime, tz = 'EST')
        id_temp$exitTime <- as.POSIXct(id_temp$exitTime, tz = 'EST')
        
        if(nrow(id_temp) != 0){ # only continue if this individual has data for this tree
          
          for(i in 1:nrow(id_temp)){ # for every time they enter a tree
            # add one to the count data for their species in our new dataframe for every second between when they enter and exit the tree
            temp_df[temp_df$timestamp >= id_temp$entranceTime[i] & temp_df$timestamp <= id_temp$exitTime[i], g_specie] <- temp_df[temp_df$timestamp >= id_temp$entranceTime[i] & temp_df$timestamp <= id_temp$exitTime[i], g_specie] + 1
          }
        }
      }
    }
  }
  
  # add the collared coatis to the coatis column, remove the collared coatis column
  temp_df$Coati_cam <- temp_df$Coati_cam + temp_df$Coati.collared_cam
  temp_df <- temp_df[,!names(temp_df) == 'Coati.collared_cam']
  
  # and write the csv
  write.csv(temp_df, paste("C:/Users/salavi/Documents/combined_DFs/" , patch_name, ".csv", sep = ''), row.names = F)
}


######################## Checkpoint 1 ######################
## Don't forget that write csv function is hashed out in loop above, so remove that if you run the last section

## read in the dataframes that we just created above
files <- list.files(path = "C:/Users/salavi/Documents/combined_DFs/", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
dat_list <- lapply(files, read.csv)
summary(dat_list)

## make a new dataframe. This is going to be our final waiting times dataframe 
final <- as.data.frame(matrix(NA, nrow = 0, ncol = 5 + length(gps_species)*5))
monk_names <- str_split(gps_species,"_", simplify = T)[,1]
names(final) <- c('patchID', 'species', 'ent_time','ext_time','wait_time', paste(monk_names, 'in_tree',sep = '_'), paste('ent_time',monk_names,sep = '_'), paste('ext_time',monk_names,sep = '_'), paste('time_since',monk_names, 'ent',sep = '_'),  paste('time_since',monk_names, 'ext',sep = '_'))

## going one tree at a time, build our waiting times dataframe
for(temp_df in dat_list){ #for each tree...
  # make the timestamp POSIX class
  temp_df$timestamp <- as.POSIXct(temp_df$timestamp, tz = 'EST')
  
  ## Change from count to presence/absence. So if the value in the column is greater than 1, just make it 1
  for(column in 3:ncol(temp_df)){
    temp_df[,column][temp_df[,column] > 1] <- 1
  }
  
  # saves the dataframe the way it currently is before we modify it
  saved_df <- temp_df
  
  # change dataframe to get entrance and exit times, rather than presence/absence
  for(column in 3:ncol(temp_df)){ # for each species
    
    temp_df[,column] <- c(NA,diff(temp_df[,column])) # takes the backward difference to highlight times when a species entered or exited a tree
  }
  
  ## find all indices where an species entered a tree
  enter <- which(temp_df == 1, arr.ind = T)
  
  ## find all indices where an species exited a tree
  exit <- which(temp_df == -1, arr.ind = T)
  exit[, 1] <- exit[, 1] - 1 # minus 1 here because these are actually the times where there is the first picture without the animal in it
  
  # separate monkey entrances/exits and the entrances/exits of other species
  inds <- which(sapply(names(temp_df),FUN = function(x) grepl('_gps', x))) # pulls out which columns correspond to the monkeys
  
  # pull out monkey entrance indices
  enter_m <- enter[enter[, 2] %in% inds,]
  
  # pull out non-monkey entrance indices
  enter <- enter[!enter[, 2] %in% inds,]
  
  # pull out monkey exit indices
  exit_m <- exit[exit[, 2] %in% inds,]
  
  # pull out non-monkey exit indices
  exit <- exit[!exit[, 2] %in% inds,]
  
  # makes a final waiting times data frame for this particular tree
  emp <- as.data.frame(matrix(NA, nrow = nrow(enter), ncol = 5 + length(gps_species)*5))
  
  # pull out the monkey species names
  monk_names <- str_split(gps_species,"_", simplify = T)[, 1]
  
  # create the names of the dataframe pulling from the monkey species names
  names(emp) <- c('patchID', 'species', 'ent_time', 'ext_time', 'wait_time', paste(monk_names, 'in_tree', sep = '_'), paste('ent_time', monk_names, sep = '_'), paste('ext_time', monk_names, sep = '_'), paste('time_since', monk_names, 'ent', sep = '_'), paste('time_since', monk_names, 'ext', sep = '_'))
  
  # pulls out the columns (the species) that has entrances into the tree
  columns <- unique(enter[, 2])
  
  # labels the name of the tree
  emp$patchID <- temp_df[enter[, 1], 'patch_ID']
  
  # pulls the names of the species that entered the tree
  emp$species <- str_split(names(temp_df)[enter[, 2]] , '_', simplify = T)[, 1]
  
  # pulls the entrance times and the exit times
  emp$ent_time <- temp_df$timestamp[enter[, 1]]
  emp$ext_time <- temp_df$timestamp[exit[, 1]]
  
  for(specie in gps_species){ # for each monkey species...
    # pull the name of the species
    spec <- str_split(specie,'_',simplify = T)[, 1] 
    
    # determine which column corresponds to this species in the full camera trap dataframe
    col2use <- which(grepl(spec, names(temp_df), ignore.case = T))
    
    # determine the 5 columns to fill with information from the species based on names of the dataframe
    col2fil1 <- which(grepl(spec, names(emp), ignore.case = T))[1]
    col2fil2 <- which(grepl(spec, names(emp), ignore.case = T))[2]
    col2fil3 <- which(grepl(spec, names(emp), ignore.case = T))[3]
    col2fil4 <- which(grepl(spec, names(emp), ignore.case = T))[4]
    col2fil5 <- which(grepl(spec, names(emp), ignore.case = T))[5]
    
    # determine whether a monkey of the given species was in the tree or not when the animals entered the tree and fill that as a 0 or 1 in "monkeySpecies_in_tree" column
    emp[, col2fil1] <- saved_df[, col2use][enter[, 1]]
    
    if(class(enter_m) != 'matrix'){ # if there is only one monkey entrance into the tree
      
      # pull the entrances of this monkey species
      sub_enter_m <- enter_m[enter_m[2] == col2use] # pull the entrances of the one 
      # pull the exits of this monkey species
      sub_exit_m <- exit_m[exit_m[2] == col2use]
      
    }else{ # do the same as above (now fitted for a matrix if there is more than one monkey entrance into the tree)
      
      sub_enter_m <- enter_m[enter_m[,2] == col2use,]
      sub_exit_m <- exit_m[exit_m[,2] == col2use,]
    }
    
    # take the difference between the time of this monkey species entrances and that of each entrance of all other species
    if(class(sub_enter_m) != 'matrix'){
      diff_mat <- outer(enter[,1], sub_enter_m[1], '-') 
    }else{
      diff_mat <- outer(enter[,1], sub_enter_m[,1], '-') 
    }
    
    # find the indices of the most recent entrance of a monkey that occured before (or at the same time as) the other species entered the tree
    temp_list <- apply(diff_mat, 1, function(x) which.min(x[x >= 0]))
    temp_list <- lapply(temp_list, FUN = function(x) if(length(x) == 0) return(NA) else return(x))
    inds1 <- unlist(temp_list)
    
    # find the time associated with these indices. This is the time of previous monkey entrance into the tree before an animal entered the tree
    prev.arrival.time <- temp_df$timestamp[sub_enter_m[inds1]]
    
    # find the time associated with the monkey's previous exit from the tree (the exit following the entrance above; which could technically be after the other animal's entrance, if both the monkey and the other animal are in the tree at the same time)
    prev.leave.time <- temp_df$timestamp[sub_exit_m[inds1]]
    
    # find the time between the animal's arrival in the tree and the monkey species' previous arrival to this tree
    time.since.arrival <- as.numeric(emp$ent_time - prev.arrival.time, units = 'secs')
    
    # find the time between the animal's arrival in the tree and the monkey species' previous (or subsequent, if they are in the tree at the same time) exit from the tree
    time.since.depart <- as.numeric(emp$ent_time - prev.leave.time, units = 'secs')
    
    # take these variables we just calculated (time monkey previously arrived and exited the tree, and the time between the monkey arrival/exit and the other animal's arrival) and save them in our waiting times dataframe
    emp[,col2fil2] <- as.POSIXct(prev.arrival.time, tz = 'EST')
    emp[,col2fil3] <- as.POSIXct(prev.leave.time, tz = 'EST')
    emp[,col2fil4] <- time.since.arrival
    emp[,col2fil5] <- time.since.depart
  }
  
  # for the species from the camera trap data that entered the tree, calculate the waiting times (time between previous exit from the tree and next entrance to the tree)
  for(column in columns){ # for each species
    
    # subset to the rows of that are for that species in the (partial) final wait times dataframe (i.e. 'emp')
    temp <- emp[enter[, 2] == column,] 
    
    # calculate the wait times
    temp$wait_time <- c(NA,temp$ent_time[-1] - temp$ext_time[-nrow(temp)])
    
    # input these into the dataframe
    emp[enter[,2] == column,] <- temp
  }
  
  ## Add the data from this tree to the whole aggregated data set
  final <- rbind(final, emp)
}

head(final)

col_names <- names(final)

final_saved <- final

# for each monkey species, find the time since their last fix prior to fix in which they were recorded entering the tree, and the time to the next fix after the fix they were recorded leaving the tree. This provides information on the uncertainty around these time recordings, which might be useful later
for(specie in gps_species){ # for each monkey species...
  
  # pull the name of the species
  spec <- str_split(specie, '_', simplify = T)[,1] 
  
  # find the columns associated with that monkey species' entrance and exit times in the tree
  col2merge <- which(grepl(spec, names(final), ignore.case = T))[2:3]
  
  # subset the monkey entrance/exit gps data to the data for this species, and the relevant columns that I need
  spec_sub <- df_gps[df_gps$Species == specie, names(df_gps) %in% c('entranceTime', 'exitTime', 'PatchID', "time_since_fix_ent" , "time_till_fix_ext")]
  
  # merge the dataframes so that we can get the time since the previous fix before entering and the time until the next fix after exiting. The merge allows us to put this info directly into our new dataframe
  final <- merge(x = final, y = spec_sub, by.x = c('patchID',names(final)[col2merge]), by.y = c('PatchID', 'entranceTime','exitTime'), all.x = T)
  
  # name the two columns we just added via the merge
  names(final)[(ncol(final)-1):ncol(final)] <- paste(spec,c("time_since_fix_ent", "time_till_fix_ext"), sep = '_')
}

head(final)
names(final)

## reorder the columns
final <- final[,c("patchID", "species", "ent_time", "ext_time", "wait_time", "Capuchin_in_tree", "Spider.monkey_in_tree", "ent_time_Spider.monkey", "ext_time_Spider.monkey", "ent_time_Capuchin", "ext_time_Capuchin", "time_since_Capuchin_ent", "time_since_Spider.monkey_ent", "time_since_Capuchin_ext", "time_since_Spider.monkey_ext", "Capuchin_time_since_fix_ent", "Capuchin_time_till_fix_ext", "Spider.monkey_time_since_fix_ent", "Spider.monkey_time_till_fix_ext")]

## reorder the rows
final <- final[order(final$ent_time),]
final <- final[order(final$species),]
final <- final[order(final$patchID),]

head(final)

## write the csv
write.csv(final,"wait_times.csv", row.names = F)

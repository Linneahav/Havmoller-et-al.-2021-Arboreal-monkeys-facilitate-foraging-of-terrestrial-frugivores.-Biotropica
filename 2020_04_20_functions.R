

##### Dyadic Distance Functions #####

#### trim_data removes the unnecessary columns from a dataframe from Movebank, renames the columns, and sets the class of each column as needed.

# df_from_mb is a dataframe downloaded directly from Movebank with the column names unchanged. The dataframe needs to include the study-local-timestamp column
# id is a character string, either 'name' or 'tag', and it determines whether individuals will be denoted by the 'individual-local-identifier' column or by the 'tag-local-identifier', respectively. User inputs in later functions need to match the individual notation chosen here.
# tz is the timezone of the local study time in the format required by lubridate's function with_tz

trim_data <- function( df_from_mb, id = 'name', tz = 'America/Panama' ){
  require( 'lubridate' )
  
  if( id == 'name' ){
    # keep just the relevent columns, with the id column being the animal's name
    df_from_mb <- df_from_mb [ , c('individual.local.identifier', 'timestamp', 'location.long', 'location.lat', 'individual.taxon.canonical.name')]
    
  }else{
    # keep just the relevent columns, with the id column being the animal's tag number
    df_from_mb <- df_from_mb [ , c('tag-local-identifier', 'timestamp','location-long', 'location-lat', 'individual-taxon-canonical-name')]
                               
  }
  
  # rename the columns to simpler names
  names( df_from_mb ) <- c( 'id', 'timestamp', 'lon', 'lat', 'species')
  
  # make the character string columns class character
  df_from_mb [, c(1, 5)] <- apply(df_from_mb [, c(1, 5)], 2, as.character)
  
  # make the timestamp into class POSIX
  df_from_mb$timestamp <- as.POSIXct( df_from_mb$timestamp, tz = 'UTC')
  
  # switch timestamp to local time
  df_from_mb$timestamp <- with_tz( df_from_mb$timestamp, tzone = tz )
  
  # artificially change the tz to UTC without actually adjusting the times accordingly. Keeping the tz as UTC makes the analysis run smoother
  df_from_mb$timestamp <- as.POSIXct( as.character( df_from_mb$timestamp ), tz = 'UTC')
  
  
  # remove rows with unsuccessful GPS fixes
  df_from_mb <- df_from_mb[ !is.na( df_from_mb$lat ), ]
  
  # for cleanliness, ensure that rows are in the correct time order and individuals are in alphatical order in the dataframe
  df_from_mb <- df_from_mb[ order( df_from_mb$timestamp ), ]
  df_from_mb <- df_from_mb[ order(as.factor( df_from_mb$id ) ),]
  
  return( df_from_mb )
}


#### std_time adds a column to the dataframe that denotes in which time period of the study the observation was collected. For each study period, the function creates a standardized set of times to which the timestamps can be rounded in order to calculate dyadic distances.

## df is the dataframe of gps locations (likely the output of the trim_data function)
## time_period_df is a user-input dataframe that has three columns, and as many rows as there are non-overlapping data collection periods. Each row represents a data collection perdiod, with the first column representing the timestamp of the beginning (or before the beginning) of a data collection period and the second column representing the timestamp of the end (or after the end) of the data collection period, and the third timestamp representing the sampling interval as a character string with units (e.g. "15 mins"); time unit must be understandable by round_date function. Timestamps should be character strings (not class POSIX) in the study local time. For studies that have only one data collection period, time_period_df should only have one row

std_time <- function( df, time_period_df = NULL, samp_int = NULL ){
  
  if( is.null( time_period_df ) & is.null( samp_int ) ){
    stop( 'either time_period_df or samp_int must be given as input')
  }
  
  
  # create an empty column that we will fill to denote the time period
  df$period <- NA
  
  # create an empty column that we will fill in with the standardized timestamps to which the current timestamps round
  df$std_timestamp <- NA
  
  if( is.null( time_period_df ) ){
    
    df$period <- 1
    
  }else{
    
    # turn the dataframe of character strings into class POSIX
    time_period_df[ , 1 ] <- as.POSIXct( time_period_df[ , 1 ], tz = 'UTC')
    time_period_df[ , 2 ] <- as.POSIXct( time_period_df[ , 2 ], tz = 'UTC')
    
    
    for( i in 1:nrow( time_period_df ) ){
      # fill in period the column with a numeric indicating which discrete time period the observation was taken during.  
      df$period [ (df$timestamp >= time_period_df [ i, 1 ]) & (df$timestamp <= time_period_df [ i, 2 ]) ] <- i

    }
    
  }
  
  df$std_timestamp <- round_date(df$timestamp, paste( as.character( time_period_df [ i, 3 ] ), 'mins'))
  
  # turn new standardized timestamps into class POSIX 
  df$std_timestamp <- as.POSIXct( df$std_timestamp, origin = '1970-01-01', tz = "UTC")
  
  if( sum( is.na( df$period ) ) > 0 ){
    # if there are any NAs in the period column, indicate that some of the observations fall outside of the input periods
    print( "One or more observations fall outside of the given periods")
  }
  
  hist( as.numeric( abs( df$std_timestamp - df$timestamp)), breaks=100, xlab='Difference between timestamp and standardized timestamp (sec)', main="How far off are the fixes?") ## This shows the histogram of the differences between the real time and the rounded time. As long as most of these are pretty close to zero, we are fine to round the times to get simultaneous fixes in this way.
  
  return( df )
}


#### lonlat_to_utm converts to coordinates from longitude/latitute coordinates to UTM coordinates

## df is the dataframe containing the coordinates to be converted
## crs_utm is the proj4string of the UTMs to which the long lat coordinates will be converted 

lonlat_to_utm <- function( df, crs_utm = CRS("+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")){
  
  print( "Columns for latitude and longitude must be names lat and lon respectively" )
  
  crs_longlat <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  
  df_sp <- SpatialPointsDataFrame(coords = df[, c('lon','lat')], proj4string = crs_longlat, data = df) 
  
  df_sp <- spTransform(df_sp, crs_utm)
  
  df <- as.data.frame(df_sp)
  
  names(df) [ names(df) == c('lon.1') ] <- 'x' 
  names(df) [ names(df) == c('lat.1') ] <- 'y' 
  
  return( df )
}


#### rem_dups removes rows that rounded to the same timestamp and are therefore duplicates

## df is the dataframe with location data output from the previous function

rem_dups <- function( df ){
  
  # split the dataframe into a list of dataframes. Each element of the list will be the data for a single individual)
  df_list <- split( df, f = df$id )
  
  # Make a new list to fill with the individuals' data with the duplicates removed
  new_df_list <- vector( 'list', length = length( df_list ) )
  
  for( n in 1:length( df_list ) ){
    
    ind <- which( duplicated( df_list [[ n ]]$std_timestamp) ) # find the duplicate timestamps
    
    if( length( ind ) > 0){ # if there are duplicates...
      
      for( m in 1:length( ind ) ){ 
        # loop through the following for the number of duplicates that there are
        
        # find the index of the first duplicate timestamp
        temp_ind <- min( which( duplicated( df_list[[ n ]]$std_timestamp ) ) ) 
        
        # find the standardized time associated with that index
        dup_time <- df_list[[ n ]]$std_timestamp[ temp_ind ] 
        
        # subset the rows that have this duplicated timestamp
        sub_df <- df_list[[ n ]][ df_list[[ n ]]$std_timestamp == dup_time, ] 
        
        # find the fix that, of this subset, is furthest in time from the standardized time
        fix_to_drop <- sub_df$timestamp[ which.max( abs( sub_df$timestamp - sub_df$std_timestamp ) ) ] 
        
        # remove it from the dataframe
        df_list[[ n ]] <- df_list[[ n ]][ df_list[[ n ]]$timestamp != fix_to_drop, ] 
        
      }
      
    }
    # after removal of duplicates, put this individual's data into the new list
    new_df_list[[n]]<- df_list[[ n ]] 
  }
  
  for( dat in new_df_list ){  ## This loop shows that there are no more duplicated timestamps
    print( which( duplicated( dat$timestamp ) ) )
  }
  
  # turns this list of individuals' dataframes back into one complete dataframe
  df <- ldply( new_df_list )
  
  return( df )
}


#### na_min is an alternative to the min function. It is a function that returns the minimum of a vector, but it will return NA (not Inf) if all of the entries in the vector are NA
# vec is a vector of numbers

na_min <- function( vec ){
  
  if( sum( !is.na( vec ) ) == 0){
    
    return( NA )
    
  }else{
    
    return( min( vec, na.rm = T) )
  }
}

#### dyad_dist takes a dataframe of locations for individuals representing a dyad. There can be more than two individuals, but they must represent only two non-redundant individuals (for example, two individuals in the same cohesive social group can be considered redundant individuals)

## df is the dataframe of locations of the individuals for which dyadic distances will be calculated
## group_col is the name of the column in the dataframe that indicates which individuals are redundant with each other. Each individual should have an integer or character that indicates to which functional group it belongs. If there are only two individuals in the dataframe, a group_col can be set to NULL

dyad_dist <- function( df, group_col = NULL ){
  
  tag_names <- as.character( unique( df$id ) )
  
  if( is.null( group_col ) & length( tag_names ) > 2 ){
    stop( "group_col must indicate the column name of indicating group membership if there are more than two individuals in the dataframe")
  }
  
  if( length( unique( df [ , group_col ] ) ) > 2 ){
    stop( "there can only be two different groups to calculate dyadic distances")
  }
  
  # for dataframes that only contain two individuals, add a group col with a unique indicator for both individuals
  if( is.null( group_col ) ) {

    # indicate that the id column should be used to differentiate groups in later steps
    group_col <- 'id'
    
  }
  
  # ensure that group_col column is of class character
  df [ , group_col ] <- as.character( df [ , group_col ] )
  
  # extract the tag_names from individuals in both groups
  a_names <- as.character( unique( df [ df [ , group_col ] == as.character( unique( df [ , group_col ] )[1] ) , 'id' ] ) )
  
  b_names <- as.character( unique( df [ df [ , group_col ] == as.character( unique( df [ , group_col ] )[2] ) , 'id' ] ) )
  
  # switch to wider format; this will make it faster to calculate dyadic distance
  x_coords <- dcast( df, std_timestamp ~ id, value.var = 'x', drop = F )
  y_coords <- dcast( df, std_timestamp ~ id, value.var = 'y', drop = F )
  
  # create an empty matrix that we will fill with dyadic distances between the groups at each time point (for every intergroup dyadic pair)
  dyad_dist_mat <- matrix( NA, nrow = nrow( x_coords ), ncol = ( length( a_names) * length( b_names ) ) )
  
  # instatiate a counter. This will determine which column of the matrix is filled
  counter <- 1
  
  for( id_A in a_names ){
    
    for( id_B in b_names ){
      
      # and calculate the distances between each intergroup dyadic pair at every time point
      dyad_dist_mat[ , counter ] <- sqrt((x_coords[ , id_A ] - x_coords[ , id_B ])**2 + (y_coords[ , id_A ] - y_coords[ , id_B ])**2) 
      
      # add to the counter
      counter <- counter + 1
    }
    
  }
  # erase the counter
  rm( counter )
  
  dyad_dist_df <- data.frame( std_timestamp = x_coords$std_timestamp, dyad_dist = apply( dyad_dist_mat, 1, FUN = na_min ) )
  
  # remove time points when at least one of the individuals does not have data
  dyad_dist_df <- dyad_dist_df[ !is.na( dyad_dist_df$dyad_dist ), ]
  
  # reoder by timestamp (not sure if this is completely necessary)
  dyad_dist_df <- dyad_dist_df[ order( dyad_dist_df$std_timestamp ), ]
  
  return( dyad_dist_df )
}


#### path_rand is the function that actually randomizes the paths to 
## df is  the dataframe containing the GPS points of individuals to be randomized. The column for individual ID must be named "id" at least for now
## n is the number of randomizations
## dist_thresh is the threshold under which animals are considered to be "associating"
## rand_day_thresh is the number of days that will be in one chunk to be permuted
## min_to_rand is the fraction (aka beteen 0 and 1) of the total possible days in one chunk of data (i.e. the rand_day_thresh) that needs to represented in the data in order for the data to be permuted. The default is set to 0, so that the chunk will be permuted regardless of how much data is missing from one of the functuals in the dyad
## id_data is a dataframe with individual ID in the first column (must match individual ID's in )
## comp_matrix is a matrix with rows and columns representing individual ID's (apparently they must be in numerical or alphabetical order). The matrix should be binary indicating which categories of individuals comparisons should be made between
## spat_polygons is a spatial polygons dataframe that with polygons that are regions if interest (to see how animals interact more or less than expected by chance in these regions)
## crs_utm is the CRS of the UTM zone of the spatial polygons
## dist_to_polys is the distance threshold in meters that animals must be equal to or under in order to be considered in the region of interest denoted by the spatial polygons

path_rand <- function( df, n = 1000, dist_thresh, rand_day_thresh, min_to_rand = 0, id_data = NULL, comp_matrix = NULL, spat_polygons = NULL, crs_utm = CRS("+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), diurn_start = NULL, diurn_end = NULL, dist_to_polys = NULL, nocturnal_df = NULL, nocturn_start = NULL, nocturn_end = NULL ){
  
  # if there are user-input polygons, make sure the crs will be the same used for the crs of the gps locations of the animals
  if( !is.null( spat_polygons ) ){
    
    spat_polygons <- spTransform( spat_polygons, crs_utm )
    
  }
  
  if( is.null( id_data ) ){
    
    # if there's no user-input id_data, just take each individual to be the functional individual (i.e. assume none of the individuals are redundant)
    df$func_ind <- df$id
    
    # assume that the categories to determine relationships between are at the species level
    df$cat <- df$species
    
  }else{
    
    # rename the columns of id_data
    names( id_data ) <- c( 'id', 'func_ind', 'cat' )
  
    # merge the id_data with the rest of the dataframe
    df <- merge( df, id_data, by = 'id', all.x = T, all.y = F, sort = F)
  }
  
  # ensure the following columns are class character
  df$id <- as.character( df$id )
  
  df$func_ind <- as.character( df$func_ind )
  
  df$cat <- as.character( df$cat )
  
  if( !'period' %in% names( df ) ){
    
    ## if there is no user-input time periods of the study, assume that all of the data comes from the same time period
    df$period <- 1
    
  }
  
  # define the unique time periods
  time_periods <- as.integer( as.character ( unique( df$period ) ) )
  
  if( is.null( comp_matrix ) ){
    
    # if no user-input comp_matrix is given, assume that the user wants a pairwise comparison of every category of animal. This is represented by a one in every position of the upper triangle of the comp_matrix (diagonal included) 
    comp_matrix <- matrix( 0, nrow = length( unique( df$cat ) ), ncol = length( unique( df$cat ) ), dimnames = list( unique( df$cat ), unique( df$cat ) ) )
    
    comp_matrix [ upper.tri( comp_matrix, diag = T ) ] <- 1
    
  }else{
    
    # set the lower triangle of the matrix to 0 so as not to repeat any dyadic comparisons redundantly
    comp_matrix [ lower.tri( comp_matrix, diag = F ) ] <- 0
    
  }
  
  # produce a matrix of the indices from the comb_matrix where there is a 1. These are the categories that we need to test for positive or negative association between
  combs <- which( comp_matrix == 1, arr.ind = T )
  
  # create empty vectors. These vectors will be filled with entries that eventually be put into the final dataframe that declares which functuals need to be compared
  vec_a <- c()
  vec_b <- c()
  vec_t <- c()
  vec_cat_a <- c()
  vec_cat_b <- c()
  
  for( t in time_periods ){
    
    # subset the dataframe of GPS locations by time period
    sub_df <- df [ df$period == t, ]
    
    for( r in 1:nrow( combs ) ){
    
      if( combs[ r, 1 ] == combs[ r, 2 ] ) {
        
        # if this entry in the comb_matrix is on the diagonal, that implies one category is being compared with itself (i.e. do individuals of species X associate with each other more or less than expected by chance)
        
        # save the names of the functuals that are in this category
        func_names <- as.character( unique( sub_df [ sub_df$cat  == rownames( comp_matrix )[ combs[ r, 1 ] ] , 'func_ind' ] ) )
        
        for( a in 1:( length( func_names ) - 1 ) ){
          
          for( b in ( a + 1 ): length( func_names ) ){
            
            # create vectors that represent the unique combinations that can be made of functuals in this category
            vec_a <- c( vec_a, func_names[ a ] )
            
            vec_b <- c( vec_b, func_names[ b ] )
            
            # create vectors that represent the categories of the individuals represented in vec_a and vec_b respectively. This could be done outside of the loop or with a merge function later, but it is simpler to just do it here, and this loop will take almost no time to run so there is barely a time cost of doing in the loop.
            vec_cat_a <- c( vec_cat_a, rownames( comp_matrix )[ combs[ r, 1 ] ] )
            
            vec_cat_b <- c( vec_cat_b, colnames( comp_matrix )[ combs[ r, 2 ] ] )
            
            # create a vector that keeps track of which time period of the data these functuals are in
            vec_t <- c( vec_t, t )
            
          }
        }
          
      }else{
        
        # if the 1 entry in the comb_matrix is not on the diagonal (i.e. combs[ r, 1 ] == combs[ r, 2 ]), two different categories are being compared (i.e. do inviduals of species X associate more expected than by chance with individuals of species Y)
        
        # save the names of the functuals in the first category of animals
        func_a_names <- as.character( unique( sub_df [ sub_df$cat == as.character( rownames( comp_matrix )[ combs[ r, 1 ] ] ), 'func_ind' ] ) )
        
        # save the names of the functuals in the second category of animals
        func_b_names <- as.character( unique( sub_df [ sub_df$cat == as.character( colnames( comp_matrix )[ combs[ r, 2 ] ] ) , 'func_ind' ] ) )
        
        for( name_a in func_a_names ){
          
          for( name_b in func_b_names ){
            
            # create vectors that represent the unique combinations that can be made of functuals in this category
            vec_a <- c( vec_a, name_a )
            
            vec_b <- c( vec_b, name_b )
            
            # create vectors that represent the categories of the individuals represented in vec_a and vec_b respectively. This could be done outside of the loop or with a merge function later, but it is simpler to just do it here, and this loop will take almost no time to run so there is barely a time cost of doing in the loop.
            vec_cat_a <- c( vec_cat_a, rownames( comp_matrix )[ combs[ r, 1 ] ] )
            
            vec_cat_b <- c( vec_cat_b, colnames( comp_matrix )[ combs[ r, 2 ] ] )
            
            # create a vector that keeps track of which time period of the data these functuals are in
            vec_t <- c( vec_t, t )
            
          }
        }
      }
    }
  }
  
  # set up the final dataframe that will be filled out to give the empirical coefficient of associations between each dyad of functuals
  real_final <- data.frame( func_a = vec_a, func_b = vec_b, time_period = vec_t, num = rep( NA, times = length( vec_a ) ), denom = rep( NA, times = length( vec_a ) ), cat_a = vec_cat_a, cat_b = vec_cat_b , stringsAsFactors = F)
  
  # set up the final dataframe that will be filled out to give the coefficient of associations between each dyad of functuals produced by the randomizations
  rand_final <- data.frame( func_a = rep( vec_a, times = n ), func_b = rep( vec_b, times = n ), time_period = rep( vec_t, times = n ), rand_n = rep( 1:n , each = length( vec_a ) ), num = rep( NA, times = length( rep( vec_a, times = n ) ) ), denom = rep( NA, times = length( rep( vec_a, times = n ) ) ), cat_a = rep( vec_cat_a, times = n ), cat_b = rep( vec_cat_b, times = n ), stringsAsFactors = F)
  
  # set up the final dataframe that will contain the empirical coefficients of association limited to when functuals are considered to be within the region of interest defined by the spatial polygons
  poly_real_final <- data.frame( func_a = vec_a, func_b = vec_b, time_period = vec_t, num = rep( NA, times = length( vec_a ) ), denom = rep( NA, times = length( vec_a ) ), cat_a = vec_cat_a, cat_b = vec_cat_b, stringsAsFactors = F )
  
  # set up the final dataframe that will contain the coefficients of association within the regions of interest that are produced by the randomizations
  poly_rand_final <- data.frame( func_a = rep( vec_a, times = n ), func_b = rep( vec_b, times = n ), time_period = rep( vec_t, times = n ), rand_n = rep( 1:n , each = length( vec_a ) ), num = rep( NA, times = length( rep( vec_a, times = n ) ) ), denom = rep( NA, times = length( rep( vec_a, times = n ) ) ), cat_a = rep( vec_cat_a, times = n ), cat_b = rep( vec_cat_b, times = n ), stringsAsFactors = F )
  
  # create a dataframe that will contain the metadata for the randomizations
  meta_df <- data.frame( func_a = vec_a, func_b = vec_b, time_period = vec_t, start_rand_at = rep( NA, length( vec_a ) ), end_rand_at =  rep( NA, length( vec_a ) ), cat_a = vec_cat_a, cat_b = vec_cat_b, stringsAsFactors = F )
  
  # saves the first day of the study
  start_date <- as.Date( min( df$std_timestamp ) )
  
  # save the time as its own column. This will be useful when creating fake timestamps during the randomizations
  df$time <- str_split_fixed( df$std_timestamp, ' ', 2)[ , 2 ]
  
  for( row in 1:nrow( real_final) ){
    
    print( row / nrow(real_final) )
    
    # subset the full gps dataframe to just including the functual dyad's data during the correct period. This appropriate combination is determined by the set up of the dataframes above
    pair_df <- df[ df$func_ind %in% c( real_final[ row, c('func_a', 'func_b') ]) & df$period == real_final[ row, 'time_period' ], ]
    
    if( !is.null( nocturnal_df ) ){
      
      if( nocturnal_df [ nocturnal_df[ , 1] == real_final[ row, 'cat_a' ], 2 ] == 1 ){
        
        if( !is.null( nocturn_start ) ){
          
          pair_df <- pair_df[ ( pair_df$time >= nocturn_start ) | ( pair_df$time <= nocturn_end ), ]
          
        }
        
        pair_df$std_timestamp <- pair_df$std_timestamp - 12*60*60
        
      }else{
        
        if( !is.null( diurn_start ) ){
         
          pair_df <- pair_df[ ( pair_df$time >= diurn_start ) & ( pair_df$time <= diurn_end ), ]
          
        }
      } 
    } else {
      
      if( !is.null( diurn_start ) ){
        
        pair_df <- pair_df[ ( pair_df$time >= diurn_start ) & ( pair_df$time <= diurn_end ), ]
        
      }
    }
    # just making sure 'func_ind' is a character and not a factor. It messes things up if it is a factor
    pair_df$func_ind <- as.character( pair_df$func_ind ) 
    
    pair_df$day <- as.numeric( as.Date( pair_df$std_timestamp )  -  start_date + 1 , units = 'days' )
    # trims the dataframe so it starts on the first day that both members of the dyad have data. We don't want to analyze anything before this
    pair_df <- pair_df[ pair_df$day >=  max( aggregate( pair_df$day , by=list( pair_df$func_ind ), FUN = min )[ , 2 ] ) , ] 
    
    # makes a column with both of their day columns starting at 1 on the first day when they both have the data. Important for chunking up the data in the next line
    pair_df$temp_day <- as.numeric( pair_df$day - min( pair_df$day ) + 1, units = 'days' )
    
    # assign each row of data to a subset so that days of the data are only randomized within an range determined by rand_day_thresh
    pair_df$chunk_num <- ceiling( pair_df$temp_day / rand_day_thresh ) 
    
    # split up the data into the subsets created in the line above. Now we have a list of dataframes, which each dataframe corresponding to one time chunk within randomizaation is allowable
    chunked_df <- split( pair_df, f = pair_df$chunk_num )
    
    # create an empty dataframe that will represent a dyadic distance at every simultaneous fix during the current study period
    total_real <- data.frame( std_timestamp = character(), dist = numeric())
    
    # perform n permutations of the dyad's dataset
    for( i in 1:n ){
      
      # create an empty dataframe that will represent a dyadic distance at every derived simultaneous fix of the study period that results after the randomization
      total_rand <- data.frame( std_timestamp = character(), dist = numeric())
      
      # initialize rand_in_poly to zero. Additions will be made in every chunk based on the number of encounters that are close to a polygon in that chunk
      rand_in_poly <- 0
      
      # loop through each chunk and permute the data within the chunks
      for( w in 1:length( chunked_df ) ){ # For each chunk of data
        
        # save the chunk of data to the dataframe "chunk"
        chunk <- chunked_df[[w]]
        
        # save the number of days of data that individuals a and b have in this chunk
        num_unique_days_a <- length( unique( chunk[ chunk$func_ind == real_final$func_a[ row ], 'day' ] ) ) 
        num_unique_days_b <- length( unique( chunk[ chunk$func_ind == real_final$func_b[ row ], 'day' ] ) ) 
        
        # if one of the functuals has no data for this chunk, skip the rest of the body of the loop and move to the next chunk
        if( num_unique_days_a == 0 || num_unique_days_b == 0 ){
          next
        }
          
        # if either individual a or b don't have the necessary number of days worth of data as determined by the min_to_rand parameter, skip the rest of the body of the loop
        if( num_unique_days_a < (min_to_rand * rand_day_thresh)  || num_unique_days_b < (min_to_rand * rand_day_thresh) ){ 
          next
        }
        
        # The first time through, we will make the empirical dataframe of dyadic distances. We only need to do this once
        if( i == 1 ){
          
          # use the dyad_dist function to calculate the dyadic distance for this pair of functuals
          dyad_out <- dyad_dist( df = chunk, group_col = 'func_ind' )

          # add this to the running dataframe of dyadic distances over the whole study for this dyad
          total_real <- rbind( total_real, dyad_out )
          
          # save the latest time that will be successfully randomized. For the first run through for each functual dyad, this will get updated every time the conditions above are surpassed such that a chunk of data is successfully randomized. Eventually this will serve to mark the end of successful randomizations
          end_of_rand <- max(chunked_df[[w]]$timestamp)
        }
        
        # determine how many days to shift ID b's data by for the randomization
        ID_b_shifts <- sample( 0:( rand_day_thresh - 1 ), 1, replace = TRUE )
        
        # shift ID b's data by the amount determined above
        new_days_b <- chunk[ chunk$func_ind == real_final$func_b[ row ], 'day'] + ID_b_shifts
        
        # complicated line of code, but all it does is wrap the end of b's data back around to match the beginning of a's data. So if we shifted b's data by 3, a's data will still be 1, 2, 3, 4, 5, 6, 7; and b's data will be 5, 6, 7, 1, 2, 3, 4.
        new_days_b[ new_days_b > max( chunk[ chunk$func_ind == real_final$func_b[ row ], 'day' ] ) ] <- new_days_b[ new_days_b > max( chunk [ chunk$func_ind == real_final$func_b[ row ], 'day' ] ) ] - max( chunk[ chunk$func_ind == real_final$func_b[ row ], 'day' ] ) + min( chunk[ chunk$func_ind == real_final$func_b[ row ], 'day'] ) - 1
        
        # replace b's day data with these 'fake' shifted days
        chunk[ chunk$func_ind == real_final$func_b[ row ], 'day'] <- new_days_b
        
        # creates a fake date based on the fake day and the known start date of the study
        chunk$fake_date <- start_date + chunk$day - 1
        
        # creates a fake timestamp by appending the real time onto the fake date
        chunk$std_timestamp <- as.POSIXct(paste(chunk$fake_date, chunk$time, sep = ' '), tz = 'UTC')
        
        # use the dyad_dist function to calculate the dyadic distance for this pair of functuals but with functual B's data time-shifted
        dyad_out_rand <- dyad_dist( df = chunk, group_col = 'func_ind')
        
        # add this to the running dataframe of derived dyadic distances for the randomized data over the whole study
        total_rand <- rbind( total_rand, dyad_out_rand )
        
        if( !is.null (spat_polygons) ){
          
          # subset the dyadic distances to just those that are considered an encounter
          close_rand <- dyad_out_rand [ dyad_out_rand$dyad_dist < dist_thresh , ]
          
          if( nrow( close_rand ) != 0 ){
            
            # subset the randomized data in this chunk to just these times when the functuals are close to each other
            sub_chunk <- chunk[ chunk$std_timestamp %in% close_rand$std_timestamp, ]
            
            # turn the locations into a spatial points dataframe
            spts <- SpatialPoints( sub_chunk[ , c( 'x', 'y' ) ], proj4string = crs_utm )
            
            # determine which of these points are near the polygons in spat_polygons
            sub_chunk$near_poly <- as.numeric( apply(gDistance(spts, spat_polygons, byid=T), 2, min) <= dist_to_polys )
            
            # determine whether one or more of the functuals were near a polygon for each timestep that they were close
            chunk_in_poly <- aggregate( sub_chunk$near_poly, by = list( sub_chunk$std_timestamp ), FUN = max )
            
            # add the number of simultaneous fixes in which the functuals were close to each other and close to a polygon to a running total
            rand_in_poly <- rand_in_poly + sum( chunk_in_poly[ , 2] == 1 )
          }
        }
      }
      
      # if we are on the first run through the loop, save the empirical CA of the dyad
      if( i == 1 ){
        
        # adds the empirical coefficient of association for this dyad
        real_final[ row , 'num' ] <- sum( total_real$dyad_dist <= dist_thresh ) 
        
        real_final[ row , 'denom' ] <- nrow( total_real )
        
        if( !is.null( spat_polygons) ){
          
          # subset the dyadic distance dataframe to only those times during which the functuals are within the dist_thresh
          close_real <- total_real [ total_real$dyad_dist <= dist_thresh, ]
          if( nrow( close_real ) != 0 ){
            
            # subset the real data to just these times when the functuals are close to each other
            sub_pair <- pair_df[ pair_df$std_timestamp %in% close_real$std_timestamp, ]
            
            # turn the locations into a spatial points dataframe
            spts_real <- SpatialPoints( sub_pair[ , c( 'x', 'y' ) ], proj4string = crs_utm )
            
            # determine which of these points are near the polygons in spat_polygons
            sub_pair$near_poly <- as.numeric( apply( gDistance( spts_real, spat_polygons, byid=T), 2, min) <= dist_to_polys )
            
            # determine whether one or more of the functuals were near a polygon for each timestep that they were close
            in_poly <- aggregate( sub_pair$near_poly, by = list( sub_pair$std_timestamp ), FUN = max )
            
          }else{
            
            in_poly <- NULL
            
          }
          
          # adds the empirical proportion of simultaneous fixes for which this dyad was both within range of each other and within range of a region of interest, designated by the spatial polygons dataframe
          poly_real_final[ row , 'num' ] <- sum( in_poly[ , 2 ] == 1 ) 
          
          poly_real_final[ row , 'denom' ] <- nrow( total_real )
          
        }
       
        meta_df[ row, c( 'start_rand_at', 'end_rand_at' ) ] <- c( min( chunked_df [[ 1 ]]$timestamp ), end_of_rand )
      
      }

      # save the randomized CA of the functual dyad for this randomization
      rand_final[ ( row + nrow( real_final ) * ( i - 1 ) ), 'num' ] <- sum( total_rand$dyad_dist <= dist_thresh )
      
      rand_final[ ( row + nrow( real_final ) * ( i - 1 ) ), 'denom' ] <- nrow( total_rand )
      
      if( !is.null( spat_polygons ) ){
        
        # adds the randomized proportion of simultaneous fixes for which this dyad was both within range of each other and within range of a region of interest, designated by the spatial polygons dataframe
        poly_rand_final[ ( row + nrow( real_final ) * ( i - 1 ) ), 'num' ] <- rand_in_poly
        
        poly_rand_final[ ( row + nrow( real_final ) * ( i - 1 ) ), 'denom' ] <- nrow( total_rand )
        
      }
    }
  }
  
  out_poly_real_final <- real_final

  out_poly_real_final$num <- real_final$num - poly_real_final$num

  out_poly_rand_final <- rand_final
  
  out_poly_rand_final$num <- rand_final$num - poly_rand_final$num
  
  meta_df$start_rand_at <- as.POSIXct( meta_df$start_rand_at, tz = 'UTC', origin ='1970-01-01 00:00:00' )
  
  meta_df$end_rand_at <- as.POSIXct( meta_df$end_rand_at, tz = 'UTC', origin ='1970-01-01 00:00:00' )
  
  if( !is.null( spat_polygons ) ){
    
    full_list <- list( final = list( real = real_final, rand = rand_final ), poly = list( real = poly_real_final, rand = poly_rand_final ), out_poly = list( real = out_poly_real_final, rand = out_poly_rand_final ) )
    
  }else{
    
    full_list <- list( final = list( real = real_final, rand = rand_final ) )
    
  }
  
  # output the final dataframes
  return( list( results = full_list, meta_data = meta_df ) )
  
}


agg_results <- function( rands_out ){
  
  full_list <- rands_out[[ 1 ]]
  
  final_list <- vector( mode = 'list', length = length( full_list) )
  
  for( lst in 1:length( full_list ) ){
    
    real <- full_list[[ lst ]][[ 1 ]]
    
    real_agg <- aggregate( real[ , c('num', 'denom') ], by = list( real$cat_a, real$cat_b ), FUN = sum, na.rm = T)
    
    names( real_agg ) <- c( 'cat_a', 'cat_b', 'num', 'denom' )
    
    real_agg$prop <- real_agg$num / real_agg$denom
    
    rand <- full_list[[ lst ]][[ 2 ]]
    
    rand_agg <- aggregate( rand[ , c('num', 'denom') ], by = list( rand$cat_a, rand$cat_b, rand$rand_n ), FUN = sum, na.rm = T)
    
    names( rand_agg ) <- c( 'cat_a', 'cat_b', 'rand_n', 'num', 'denom' )
    
    rand_agg$prop <- rand_agg$num / rand_agg$denom
    
    final_list [[ lst ]] <- list( real_agg = real_agg, rand_agg = rand_agg )
  }
  
  if( length( full_list ) > 1 ){
    
    names( final_list ) <- c( 'final', 'poly', 'out_poly' )
    
  } else {
    
    names( final_list ) <- c( 'final' )
    
  }
  
  return( final_list ) 
}

  
  

plot_rand_results <- function( final_list ){
  
  for( list in final_list ){
    
    real <- list[[ 1 ]]
    
    rand <- list[[ 2 ]]
    
    for( i in 1:nrow( real ) ){
      
      real_val <- real[ i, 'prop' ]
      
      rand_vals <- rand[ rand$cat_a == real[ i, 'cat_a' ] & rand$cat_b == real[ i, 'cat_b' ], 'prop' ]
      
      hist(rand_vals,
           xlim = c( min( rand_vals, real_val ), max( rand_vals, real_val ) + 0.1 * max( rand_vals, real_val ) ),
           main = paste( real[ i, 'cat_a' ] , real[ i, 'cat_b' ] ),
           breaks = 40,
           xlab = paste("Proportion of fixes") )
      abline(v= real_val ,col='red')
      p <- sum( real_val < rand_vals ) / max( rand$rand_n )
      mtext( paste( 'p-value = ', round( min( p, 1-p ), 4) ), side = 4 )
    }
  }
}

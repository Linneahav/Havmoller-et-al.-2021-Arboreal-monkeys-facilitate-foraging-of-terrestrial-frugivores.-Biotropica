

# Randomization


setwd( "/Users/carterloftus/Documents/Spacing/dyadic_interactions/" )

## dataframe preparation
df <- read.csv( "Food for Thought - Comparative Frugivore Tracking Cleaned data v2.csv" )

df <- trim_data( df )

time_periods <- read.csv( "time_periods.csv" )

df <- std_time( df, time_period_df = time_periods )

df <- lonlat_to_utm( df )

df <- rem_dups( df )

Linnea_id_data <- read.csv( 'Linnea_id_data.csv' )

Linnea_comp_matrix <- read.csv( 'Linnea_comp_matrix.csv' )

Linnea_comp_matrix <- as.matrix( Linnea_comp_matrix )

rownames( Linnea_comp_matrix ) <- colnames( Linnea_comp_matrix ) 

## polygon preparation

dipteryx <- readOGR( dsn = "dipteryx_shapefile/", layer = "BCI_Dipteryx_Patches" ) 

## randomization

res <- path_rand( df = df, n = 1000, dist_thresh = 20, rand_day_thresh = 7, min_to_rand = 1, id_data = Linnea_id_data, comp_matrix = Linnea_comp_matrix, spat_polygons = dipteryx, diurn_start = "07:00:00", diurn_end = "18:00:00", dist_to_polys = 20 )

## write csv's
write.csv( res[[2]], 'rands_meta.csv', row.names = F )

top_names <- names( res[[1]] )

for( i in 1: length( res[[1]]) ){
  
  i_list <- res[[1]][[i]]
  
  sub_names <- names( i_list )
  
  for( j in 1:length( i_list ) ){
    
    write.csv( i_list[[ j ]], paste0( top_names[ i ], '_', sub_names[ j ], '.csv' ), row.names = F )
    
  }
}


## read csv's back in

final_rand <- read.csv( "Linneas results/Linnea final_rand.csv" )
final_real <- read.csv( "Linneas results/Linnea final_real.csv" )
out_poly_rand <- read.csv( "Linneas results/Linnea out_poly_rand.csv" )
out_poly_real <- read.csv( "Linneas results/Linnea out_poly_real.csv" )
poly_rand <- read.csv( "Linneas results/Linnea poly_rand.csv" )
poly_real <- read.csv( "Linneas results/Linnea poly_real.csv" )

res <- list( list( final = list( real = final_real, rand = final_rand ), poly = list( real = poly_real, rand = poly_rand ), out_poly = list( real = out_poly_real, rand = out_poly_rand ) ) , 'placeholder' )


final_res <- agg_results( res )

plot_rand_results( final_res )



### combining monkeys species

agg_results_update <- function( rands_out ){
  
  full_list <- rands_out[[ 1 ]]
  
  final_list <- vector( mode = 'list', length = length( full_list) )
  
  for( lst in 1:length( full_list ) ){
    
    real <- full_list[[ lst ]][[ 1 ]]
    
    real$cat_a <- ifelse( real$cat_a == 'Capuchin' | real$cat_a == 'Spider_monkey', 'Monkey', real$cat_a )
    
    real_agg <- aggregate( real[ , c('num', 'denom') ], by = list( real$cat_a, real$cat_b ), FUN = sum, na.rm = T)
    
    names( real_agg ) <- c( 'cat_a', 'cat_b', 'num', 'denom' )
    
    real_agg$prop <- real_agg$num / real_agg$denom
    
    rand <- full_list[[ lst ]][[ 2 ]]
    
    rand$cat_a <- ifelse( rand$cat_a == 'Capuchin' | rand$cat_a == 'Spider_monkey', 'Monkey', rand$cat_a )
    
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



final_res <- agg_results_update( res )


## PLOTS 

# First plotting the general coefficients of association
general <- final_res[[ 1 ]]

real_general <- general[[ 1 ]]
rand_general <- general[[ 2 ]]

real_val <- real_general$prop

rand_vals <- rand_general$prop

hist(rand_vals,
     xlim = c( min( rand_vals, real_val ), max( rand_vals, real_val ) + 0.1 * max( rand_vals, real_val ) ),
     main = paste( real_general$cat_a, 'and', real_general$cat_b, 'general coefficient of association' ),
     breaks = 40,
     xlab = "Coefficient of association")
abline(v= real_val ,col='red') 
p <- sum( real_val < rand_vals ) / max( rand_general$rand_n )
mtext( paste( 'p-value = ', round( min( p, 1-p ), 4) ), side = 4 )


# then plotting the probability of both being within range of each other and being near a dipteryx tree

in_poly <- final_res[[ 2 ]]

real_in_poly <- in_poly[[ 1 ]]
rand_in_poly <- in_poly[[ 2 ]]

real_in_dip <- real_in_poly$prop

rand_in_dip <- rand_in_poly$prop

hist(rand_in_dip,
     xlim = c( min( rand_in_dip, real_in_dip ), max( rand_in_dip, real_in_dip ) + 0.1 * max( rand_in_dip, real_in_dip ) ),
     main = paste( real_in_poly$cat_a,'and', real_in_poly$cat_b, 'in a dipteryx' ),
     breaks = 40,
     xlab = "Probability of being within threshold and close to dipteryx")
abline(v= real_in_dip ,col='red') 
p <- sum( real_in_dip < rand_in_dip ) / max( rand_in_poly$rand_n )
mtext( paste( 'p-value = ', round( min( p, 1-p ), 4) ), side = 4 )


# lastly, plotting the probability of both being within range of each other and NOT being near a dipteryx tree

out_poly <- final_res[[ 3 ]]

real_out_poly <- out_poly[[ 1 ]]
rand_out_poly <- out_poly[[ 2 ]]

real_out_dip <- real_out_poly$prop

rand_out_dip <- rand_out_poly$prop

hist(rand_out_dip,
     xlim = c( min( rand_out_dip, real_out_dip ), max( rand_out_dip, real_out_dip ) + 0.1 * max( rand_out_dip, real_out_dip ) ),
     main = paste( real_out_poly$cat_a,'and', real_out_poly$cat_b, 'out of dipteryx' ),
     breaks = 40,
     xlab = "Probability of being within threshold and away from dipteryx")
abline(v= real_out_dip ,col='red') 
p <- sum( real_out_dip < rand_out_dip ) / max( rand_out_poly$rand_n )
mtext( paste( 'p-value = ', round( min( p, 1-p ), 4) ), side = 4 )



## together on one plot ( both histograms )
hist(rand_in_dip,
     xlim = c( min( rand_in_dip, real_in_dip, rand_out_dip, real_out_dip ), max( rand_in_dip, real_in_dip, rand_out_dip, real_out_dip ) + 0.1 * max( rand_in_dip, real_in_dip, rand_out_dip, real_out_dip ) ),
     main = paste( real_in_poly$cat_a,'and', real_in_poly$cat_b, 'in a dipteryx' ),
     xlab = "Probability of dyad being with 20 m of each other",
     probability = F,
     breaks = 13)
abline(v= real_in_dip ,col='red') 


hist(rand_out_dip,
     xlab = "Probability of dyad being with 20 m of each other",
     add = T,
     probability = F,
     breaks = 10)

abline(v= real_out_dip ,col='red') 




## together on one plot (densities) 


transp <- function(col, alpha=.5){
  res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
}


rand_in_dens <- density( rand_in_dip )
rand_out_dens <- density( rand_out_dip )

plot(rand_in_dens$x, rand_in_dens$y,
     xlim = c( min( rand_in_dens$x, rand_out_dens$x, real_in_dip, real_out_dip ), max( rand_in_dens$x, rand_out_dens$x, real_in_dip, real_out_dip ) + 0.05 * max( rand_in_dens$x, rand_out_dens$x, real_in_dip, real_out_dip ) ),
     ylim = c( 0, max(rand_in_dens$y, rand_out_dens$y ) ),
     type = "l",
     col = "blue",
     #main = paste( real_in_poly$cat_a,'and', real_in_poly$cat_b, 'in a dipteryx' ),
     xlab = "Probability of monkey-coati dyad being with 20 m of each other",
     bty = "n",
     xaxt = "n",
     yaxt = "n",
     ylab = "Density",
     cex.lab = 1.2)

polygon( rand_in_dens$x, rand_in_dens$y, col = transp( "blue", 0.5), border = NA )

lines( rand_out_dens$x, rand_out_dens$y, col = "red" )

polygon( rand_out_dens$x, rand_out_dens$y, col = transp( "red", 0.5), border = NA )

segments( real_in_dip, 0, real_in_dip, max(rand_in_dens$y, rand_out_dens$y ) ,col='blue', lwd = 2, lty = 2) 

segments( real_out_dip, 0, real_out_dip, max(rand_in_dens$y, rand_out_dens$y )  ,col='red', lwd = 2, lty = 2) 

axis( 1, at = seq( 0, 0.0012, by = 0.0002 ), labels = c("0","2.0e-4","4.0e-4","6.0e-4","8.0e-4","1.0e-3","1.2e-3" ))

axis( 2, at = seq( 0, 20000, by = 5000 ), las = 1, labels = c( "0", "5.0e3", "1.0e4", "1.5e4", "2.0e4") ) 

legend( x = "top", legend = c( "encounter within dipteryx", "encounter outside of dipteryx"), fill = c( transp("blue"), transp("red") ) ) 



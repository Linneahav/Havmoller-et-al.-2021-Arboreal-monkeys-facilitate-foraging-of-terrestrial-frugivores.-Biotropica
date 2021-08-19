# Fit a Gaussian mixture model to log-inter-visit durations. 
# Havmoller et al. (2021) Biotropica. 

library(rstan)
library(rstantools)
options(mc.cores = parallel::detectCores())

# Data set of waiting times between visits, with binary indicators for presence/absence of monkeys in tree.
d <- read.csv("wait_times.csv") 

# Data preparation. 
d$ent_time <- as.POSIXct(d$ent_time, tz = 'EST')
d$day <- as.numeric(as.Date(d$ent_time) - as.Date(min(d$ent_time)) + 1)
d$diff_day <- c(NA, diff(d$day))
d$monk_in_tree <- as.numeric(d$Capuchin_in_tree == 1 | 
  d$Spider.monkey_in_tree == 1)

# Check species occurrences by monkey indicator. 
table(d$species, d$monk_in_tree)

# Subset full data set to exclude waiting times extending into the next day, 
# as well as records missing for day-difference or waiting time. 
final1 <- d[d$diff_day == 0 & !is.na(d$diff_day) & !is.na(d$wait_time),]
final1$monk_in_tree <- as.numeric(final1$Capuchin_in_tree == 1 | 
  final1$Spider.monkey_in_tree == 1)

### Model with Tree Random Effects

# Preparing inputs for Stan.
# Agouti is the only species with adequate data for mixture modeling. 
stan_df <- final1[final1$species == 'Agouti', c('patchID','wait_time','monk_in_tree')]
stan_df$patchID.num <- as.integer(as.factor(stan_df$patchID))
stan_df$log_wt <- log(stan_df$wait_time)
y0 <- stan_df$log_wt[ stan_df$monk_in_tree == 0 ] # Waiting times without monkey in tree. 
y1 <- stan_df$log_wt[ stan_df$monk_in_tree == 1 ] # Waiting times with monkey in tree.
tree_id0 <- stan_df$patchID.num[ stan_df$monk_in_tree == 0 ] ## Tree IDs without a monkey in the tree.
tree_id1 <- stan_df$patchID.num[ stan_df$monk_in_tree == 1 ] ## Tree IDs with a monkey in the tree.
N_trees <- length(unique(stan_df$patchID.num)) ## Number of unique trees in the data.
# Relative frequencies of occurrences of tree IDs, for marginalization of mean predictions in Stan model. 
tree_rel_freq <- as.numeric(table(stan_df$patchID.num) / nrow(stan_df)) 

mix.stan.inputs <- list('N0' = length(y0), 'N1' = length(y1), 'y0' = y0, 'y1' = y1, 
                       'tree_id0' = tree_id0, 'tree_id1' = tree_id1, 'N_trees' = N_trees, 
                       'tree_rel_freq' = tree_rel_freq)

# Stan control parameters. 
M <- 2000 # Number of total iterations of each chain.
n.chains <- 4 

# Compile model and generate posterior parameter samples, using default control settings. 
mix.stan <- stan(file='mixture_model_for_Biotropica.stan', 
  data=mix.stan.inputs, iter=M, chains=n.chains)

# Pairwise scatter plots of parameter samples-- for identifiability diagnostics. 
pairs(mix.stan, pars = c('mu', 'mu0', 'mu1', 'sigma', 'sigma_tree', 'p'))
# Plot of parameter estimates and posterior credibility intervals.  
plot(mix.stan, pars = c('mu', 'mu0', 'mu1', 'sigma', 'sigma_tree', 'p'))
# Numerical summaries of posterior distributions, combined over chains and per chain. 
summary(mix.stan, 
  pars = c('mu', 'mu0', 'mean0_marginal', 'mu1', 'mean1_marginal', 'sigma', 'sigma_tree', 'p'))

# Brief summary:
# The chains are behaving well and the compartments of the Gaussian mixture 
# seem to all be identified. There do not seem to be problems of label-switching
# that can occur with mixture models.  
# Meander waiting times ("mu") are very short, around exp(1.8) = 6 seconds,  
# and are distinguishable from the waiting times between unique visits ("mu0", "mu1"). 
# The meandering times appear to make up about half of all of 
# the waiting times. There is very modest support for distinct waiting times between 
# the monkey-in vs. monkey-out conditions ("mu0" vs. "mu1"). 
# But there is considerable statistical overlap between these waiting times. 
# Tree-to-tree variation is present, but the SD of tree effects on 
# waiting times ("sigma_tree") is small, relative to other parameters.

# Save the model object to workspace, so that it does not have to be re-run every time. 
saveRDS(mix.stan, file = "mix.stan.rds")

# Import the model object. 
mix.reloaded <- readRDS("mix.stan.rds") 

# Produce a simple summary graphic displaying the fitted model and data.

# Format parameter samples from Stan object.  
mix.draws <- as.array(mix.reloaded, pars=c('mu', 'mean0_marginal', 'mean1_marginal', 'sigma', 'sigma_tree', 'p'))
dimnames(mix.draws) <- list(iterations=dimnames(mix.draws)$iterations, 
  chains=dimnames(mix.draws)$chains, 
  parameters=c('mu', 'mean0_marginal', 'mean1_marginal', 'sigma', 'sigma_tree', 'p'))
params <- c('mu', 'mean0_marginal', 'mean1_marginal', 'sigma', 'sigma_tree', 'p')
# Divide nrow by 2, because 1/2 of Stan's iterations are discarded as warmup. 
mix.samples <- matrix(mix.draws, nrow=M*n.chains/2, ncol=length(params), byrow=FALSE)
colnames(mix.samples) <- params

# Get posterior mean parameter values. 
mix.posterior.means <- apply(mix.samples, MAR=2, FUN="mean")

# Prepare graphic.

# Function for color transparency. 
addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255,
  # 0 being fully transparant and 255 being fully visible.
  # Works either with color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

# Estimated densities from model.
xmax <- max(c(y0, y1))
log.wt.seq <- seq(from=-2, to=xmax, by=0.01)
m0.density <- mix.posterior.means["p"] * dnorm(x=log.wt.seq,
	mean=mix.posterior.means["mu"], sd=mix.posterior.means["sigma"]) +
	(1 - mix.posterior.means["p"]) * dnorm(x=log.wt.seq,
	mean=mix.posterior.means["mean0_marginal"], sd=mix.posterior.means["sigma"])
m1.density <- mix.posterior.means["p"] * dnorm(x=log.wt.seq,
	mean=mix.posterior.means["mu"], sd=mix.posterior.means["sigma"]) +
	(1 - mix.posterior.means["p"]) * dnorm(x=log.wt.seq,
	mean=mix.posterior.means["mean1_marginal"], sd=mix.posterior.means["sigma"])

### Graphical display for Biotropica manuscript, March 2021.

# Transparent red and blue for histograms and density curves. 
blue.hist <- addTrans(rep("turquoise4", length(y0)), 70) 
blue.curve <- addTrans(rep("turquoise3", length(m0.density)), 175)
red.hist <- addTrans(rep("tomato4", length(y1)), 70) 
red.curve <- addTrans(rep("tomato3", length(m1.density)), 175)

# Write graphics file. 
jpeg("mixture_model_display_for_Biotropica.jpg", width=6, height=4, units="in", res=750)
par(mar=c(4, 1, 1, 0) + 0.1, oma=c(1,0,0,0))
h0 <- hist(y0, breaks=15, plot=FALSE)
h1 <- hist(y1, breaks=15, plot=FALSE)
ymax <- max(h0$density, h1$density, m0.density, m1.density)
plot(x=log.wt.seq, y=m0.density, ylim=c(0, ymax), #xlim=c(xmin, xmax), 
     bty="n", type="l", col=blue.curve, lwd=3.5, yaxt="n", xaxt="n", ylab="", 
     xlab="Waiting time (seconds, log scale)", mgp=c(3,3,0))
hist(y0, breaks=15, probability=TRUE, col=blue.hist, border="transparent", add=TRUE, 
     xlab="", ylab="", main="", axes=FALSE)
lines(x=log.wt.seq, y=m1.density,  col=red.curve, lwd=3.5)
hist(y1, breaks=15, probability=TRUE, col=red.hist, border="transparent", add=TRUE, 
     xlab="", ylab="", main="", axes=FALSE)
axis(side=1, line=1, 
     at=c(log(1), log(5), log(25), log(250), log(2500), log(25000)), 
     labels=c("1", "5", "25", "250", "2500", "25,000"))
legend("topright", legend=c("Monkey Present", "Monkey Absent"), 
       lwd=c(3, 3), col=c("tomato3", "turquoise3"), bty="n", cex=1.0)
graphics.off()

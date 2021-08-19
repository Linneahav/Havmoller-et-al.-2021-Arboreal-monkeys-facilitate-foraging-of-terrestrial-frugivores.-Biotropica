### Poisson regression analysis of the playback experiment data. 
### Havmoller et al. (2021) Biotropica. 
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/UC Davis & STRI/PublicationScripts&Data")

library(stringr)
library(lme4)
library(Matrix)

### Data set of event counts per trial. 
PB <- read.csv("PB_posthoc.csv")

### Declare factors. 
PB$Treatment <- factor(PB$Treatment, 
                       levels = c("Pre", "Control", "Experiment"))
PB$Location <- factor(PB$Location)
PB$Time_of_day <- as.factor(PB$Time_of_day)

### Check that treatment and time of day look right.
table(PB$Treatment, PB$Time_of_day)

### Table of number of events by treatment.
table(PB$Treatment, PB$n_events)

### Basic Poisson glmm.
### Including duration as an exposure creates a singularity in the design matrix
### because duration is predictable from treatment (pre always 1h, ex always 4h).
### To convert the model estimates to properly-scaled rates, use a deterministic correction
### as shown later. 
m1.lme4 <- glmer(n_events ~ Treatment + Time_of_day + (1 | Location),
                 data=PB, family = "poisson")

### Summary results. 
### Not the final adjusted result, still need to account for the differences in duration.
summary(m1.lme4)

### Goodness of fit checks. 
m1.lme4.res <- residuals(m1.lme4, type = "pearson")
plot(log(fitted(m1.lme4)), m1.lme4.res)
sum(m1.lme4.res^2) # Pearson statistic. Compare with sample size. 

### Set up for graphical display. 

### Define the duration-corrected rates. 
### Working on the scale of the log link function, subtract log(4) to the experimental effects. 
### Thus they become contrasts with the Intercept, which corresponds to pre-treatment. 
### The pre-treatment period was 1/4 as long as the playback periods, including controls.
### Pre-treatment: 1 hour. Experimental periods: 4 hours. 
c.rate <- fixef(m1.lme4) - log(4)
### Define the standard error of the corrected rate
SE.treatment <- sqrt(diag(vcov(m1.lme4)))
names(SE.treatment) <- names(fixef(m1.lme4))
treatments <- str_detect(names(fixef(m1.lme4)),"Treat")
c.rate <- c.rate[treatments]
SE.treatment <- SE.treatment[treatments]

### Define the N(0,1) quantile giving the confidence interval width for 2 comparisons (controls and experiments), 
### using the Bonferroni correction (correctly controls for experimentwise error).
z95 <- -qnorm(p=(0.025/2))
z90 <- -qnorm(p=(0.05/2))
z85 <- -qnorm(p=(0.075/2))

### Graphical display.
n.treatments <- length(c.rate)
# Simple string placements for the two factor levels.
string.positions <- (n.treatments:1) - rep(0.5, n.treatments)

### Subtract a number from the minimum interval endpoint to make room for labels.
xmin <- min(c.rate - (z95*SE.treatment)) - 2
xmax <- max(c.rate + (z95*SE.treatment)) 
### Make plot.
jpeg("playback_posthoc.jpeg", height=6, width=8, units="in", res=750)
plot(x=c.rate, y=string.positions, bty="n", yaxt="n", xaxt="n", xlab="", ylab="", 
     xlim=c(xmin, xmax), type="n", ylim=c(0, n.treatments))
segments(x0=c.rate - (z95*SE.treatment), y0=string.positions, x1=c.rate + (z95*SE.treatment), lwd=3,
         col="grey60")
segments(x0=c.rate - (z90*SE.treatment), y0=string.positions, x1=c.rate + (z90*SE.treatment), lwd=4,
         col="blue3")
segments(x0=c.rate - (z85*SE.treatment), y0=string.positions, x1=c.rate + (z85*SE.treatment), lwd=6,
         col="dodgerblue1")
points(x=c.rate, y=string.positions, pch=19, cex=2.0, col="grey20")
treatment.string <- c("Control", "Experiment")
text(x=xmin, y=string.positions, labels=treatment.string, adj=c(0,0), cex=1.5, font=1)
abline(v=0, lty=2, col="grey80", lwd=2)
axis(side=1, cex.axis=1.25, at= c(log(1/2), log(1), log(2), log(4)), 
     labels=c("1:2", "1:1", "2:1", "4:1"))
mtext(text=c("Relative Event Rates (treatment:baseline)"), side=1, line=3, cex=1.5, font=2, adj=0.8)
graphics.off()

### Treatment event rates compared to baseline rates, with confidence intervals.
### The event rate for combined experimental and control condition is compared to the baseline 
### (pre-treatment) event rate.
### The x-axis is on the natural log-scale, though the tick marks are labeled with relative rates. 
### Confidence interval widths are corrected for two comparisons: each treatment compared with baseline.

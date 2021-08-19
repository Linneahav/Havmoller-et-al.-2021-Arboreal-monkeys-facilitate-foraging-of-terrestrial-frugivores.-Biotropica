
### Test for difference in mean duration of coati visits to dipteryx
### With known monkey presence (collared monkeys) vs absence (collared monkeys)

CoatiDipDuration <- read.csv("Library/Mobile Documents/com~apple~CloudDocs/UC Davis & STRI/Linnea STRI/CoatiDipDurationMins.csv")


my_data <- CoatiDipDuration
View(my_data)

### Go to the bottom for the final analysis ###


#my_data$Duration <- as.numeric(my_data$Duration)


# Test for normal distribution
library("ggplot2")
library("magrittr")
library("dplyr")
library("ggpubr")


######Final Test for the paper #########
# Two-sample Kolmogorov-Smirnov test
splitdata=split(my_data,my_data$Group)
ks.test(x=splitdata$Monkey_present$Duration, y=splitdata$Monkey_absent$Duration)
# Make histograms to check the distribution
hist(splitdata$Monkey_present$Duration)
hist(splitdata$Monkey_absent$Duration)

## Exponential distribution, so report the rate
library("fitdistrplus")
Fit1=fitdistrplus::fitdist(splitdata$Monkey_present$Duration,"exp")
plot(Fit1)
Fit2=fitdistrplus::fitdist(splitdata$Monkey_absent$Duration,"exp")
plot(Fit2)
library(ggplot2)

ggplot(my_data,aes(x=Group, y=Duration, fill=Group))+geom_violin(trim = T)+geom_boxplot(width=.05, outlier.shape = NA)+ theme_classic()

#### Log transforming it, Mark's suggestion #### Not included in the final paper###
my_data$log_duration <- log(my_data$Duration)

## Exponential distribution with natural log taken for all durations
ggplot(my_data,aes(x=Group, y=log(Duration), fill=Group))+geom_violin(trim = T)+geom_boxplot(width=.05, outlier.shape = NA)+ theme_classic()

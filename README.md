# Havmoller et al. 2021 Arboreal monkeys facilitate foraging of terrestrial frugivores. Biotropica
![image](https://user-images.githubusercontent.com/28358826/130823405-e2bda053-4831-4ec6-9e7c-0f2edbe9df4a.png)

This repository contains statistical code related to Havmoller et al. (2021) Arboreal monkeys facilitate foraging of terrestrial frugivores. Biotropica

[Data](https://osf.io/2adg5/) used in all analyses for this project are hosted on [Open Science Framework](https://osf.io/2adg5/). Original movement data are hosted on [Movebank](https://www.movebank.org/) (Processed data: Movebank ID 1120749252; Unprocessed data: Movebank ID 468460067)

## Instructions
There are various packages with complex dependencies used accross the scripts used in this study. In order to install every package, Windows users must first ensure that the latest version of [rtools](https://cran.r-project.org/bin/windows/Rtools/) is properly installed. Mac users must ensure that the latest version of [Xcode](https://developer.apple.com/xcode/) is properly installed.  This step cannot be skipped .

The following script installs all of the required packages and their dependencies:

```
install.packages(c("devtools","miniCRAN","pacman"), dependencies = TRUE, type="source") 
devtools::install_github("wrathematics/getPass")

#Check if required packages and their dependencies need installation or updates
list_of_required_packages <- c("mgcv", "plyr", "dplyr", "readr", "stringr", 
"lme4", "Matrix", "maditr", "reshape2", "magrittr", "ggplot2", "ggpubr", "fitdistrplus", "tidyr", 
"mgcv", "gganimate", "scales", "tweenr", "lubridate", "spacetime", "sp", 
"plotKML", "rgdal", "raster", "rgeos", "move", "recurse","RStoolbox", "ggspatial", "gapminder", "gifski", 
"StanHeaders", "rstan")

check_if_needs_install=as.character(miniCRAN::pkgDep(list_of_required_packages, suggests = TRUE, enhances = TRUE))
check_if_needs_update=as.character(pacman::p_update(FALSE))

new_packages <- check_if_needs_install[!(check_if_needs_install %in% installed.packages()[,"Package"])]
packages_to_update=check_if_needs_install[check_if_needs_install %in% check_if_needs_update]

packages_to_install=c(new_packages,packages_to_update)


install.packages(packages_to_install, type="source")
```


Be sure to change the working directory to an appropriate one on your system. Also be sure to modify where write.csv() saves all outputs. 

For ```Dyadic_dist16.R```, ```Dyadic_dist18.R```, ```KMLs.R```, and ```KMLs16.R```, please replace the movebank login information with your user information. If you are unable to download the data from movebank, you may need to log in from your browser and request access to the project. 


## Script metadata

```Dyadic_dist16.R```: Isolates segments of the 2015-16 data where there are encounters (animals within 20m of each other). Generates data to be used in ```KLM.R```

```Dyadic_dist18.R```: Isolates segments of the 2017-18 data where there are encounters (animals within 20m of each other). Generates data to be used in ```KLM.R```

```KMLs.R```: Animates the 2017-18 data as KML files to be reviewed in google earth pro and examine encounter outcomes.

```KMLs16.R```: Animates the 2015-16 data as KML files to be reviewed in google earth pro and examine encounter outcomes.

```Randomizations.R```: Permutes GPS tracks at daily timescale, to test encounter rates and durations against random. 

```2020_04_20_functions.R```: Contains functions that are called in ```Randomizations.R```, which are necessary for cleaning the data for permutation, permuting the tracks, and plotting the results.

```Playback_Analysis.R```: To statistically test experimental treatments against controls for the playback experiments. 

```Playback_posthoc_analysis_SupplementaryFigure.R```: Same as ```Playback_Analysis.R``` except treatments and controls have been combined per the suggestion of the subject editor to confirm that statistical trends are real and lack of significance a function of sample size

```CoatiDipVisitDuration_monkey_presenceVSabsence.R```: Script to make statistical comparisons of coati Dipteryx visit durations with and without the presence of collared monkeys.

```ftt recurse dip.R```: Calculates tree entry and exit times for collared monkeys, data are used in ```cam_gps_merge.R```

```cam_gps_merge.R```: Integrates data from camera traps and from GPS collars to determine both the time lag between fruit tree visits (using the camera trap data) and whether a collared monkey was in the tree during the visit. This script produces ```waiting_times.csv``` which is then supplied to ```mixture_model_for_Biotropica.R```

```mixture_model_for_Biotropica.stan```: Stan script specifying the stan model to be used in ```mixture_model_for_Biotropica.R```

```mixture_model_for_Biotropica.R```: Script to statically test the effect of monkey presence on terrestrial frugivore visit lag times. Calls on ```mixture_model_for_Biotropica.stan``` 

```mix.stan.rds```: exported STAN model resulting from ```mixture_model_for_Biotropica.R```

```animate_SupplementaryFigure.R``` generates supplementary videos S1 and S2
```BCI_HR_dip_SupplementaryFigure.R``` generates supplementary figure S1.


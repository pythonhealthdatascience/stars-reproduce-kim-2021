# SURVEILLANCE COHORT
## SCENARIO S1: varying delay to surveillance scan
## (i) All surveillance scans suspended by 3 months initially (and therefore, large AAAs continue to be monitored at interval for medium AAAs)
## (ii) Threshold for operation changed to 50cm for first 3 months (no elective ops), then revert to 5.5cm
# (iii) varying delay to surveillance scans from 3m to 5y

## Install required packages if not already installed
list.of.packages <- c("Rcpp", "expm", "msm", "foreach", "iterators", "doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(msm)
library(foreach)
library(iterators)
library(doParallel)

rm(list=ls())

## Set the working directory to the root directory of AAA_DES_model (can only run this if sourcing the file)
this.dir <- dirname(parent.frame(2)$ofile)
setwd(paste0(this.dir,"/../../"))

# Model
source("functions/DES_Model.R")

# Function to count the aorta sizes of people with AAA-related deaths
source("functions/extra_functions.R")

## Input parameters
source("input/NAAASP_Men_2020-05-11/DES_Data_Input_NAAASP_Men_30years_time_horizon_2020-05-11.R") 

## Change v1other$aortaDiameterThresholds to be a list (new syntax)
# v1other$aortaDiameterThresholds <- list(v1other$aortaDiameterThresholds)

## Also allow monitoring after contraindication (currently discharged)
v1other$monitoringIntervals[4] <- 0.25 ## 3 monthly monitoring for those contraindicated

## screening group only
v0$treatmentGroups <- "screening"

## List parameters
v0
v1other
v2
v1distributions

## Set other quantities
v0$returnEventHistories <- T ## return individual event histories
v0$returnAllPersonsQuantities <- F ## To save memory we will not return individual HE quantitites
v0$method <- "parallel"

v0$numberOfPersons <- 1e6

###############################################################################################################################################################
# SURVEILLANCE COHORT
###############################################################################################################################################################


## Those under surveillance
personData.surv <- expand.grid(startAge = 65:94, baselineDiameter = seq(3.0, 5.4, by = 0.1))
fileName <- "input/NAAASP_COVID_modelling/naaasp_men_surv_may20.csv"
temp <- read.csv(fileName, comment.char="")[,1:3]
names(temp) <- c("startAge", "baselineDiameter", "prob")
personData.surv <- merge(personData.surv, temp, by = c("startAge", "baselineDiameter"))

v2$probOfRequireReinvitation <- setType(0, "probability")
v2$probOfAttendScreen <- setType(1, "probability")
v2$probOfNonvisualization <- setType(0, "probability")
v2$costs["inviteToScreen"] <- 0
v2$costs["requireReinvitation"] <- 0
v2$costs["screen"] <- v2$costs["monitor"]


####################################################################################################################################################################
## SCENARIO S1: varying delay to surveillance scan
## (i) All surveillance scans suspended by 3 months initially (and therefore, large AAAs continue to be monitored at interval for medium AAAs)
## (ii) Threshold for operation changed to 50cm for first 3 months (no elective ops), then revert to 5.5cm
# (iii) varying delay to surveillance scans from 3m to 5y
v1other$inviteToScreenSuspensionTime <- 0
v1other$aortaDiameterThresholds <- list(c(3, 4.5, 50), c(3, 4.5, 5.5), c(3, 4.5, 5.5))
attr(v1other$aortaDiameterThresholds, "timeCutPoints") <- c(0.25, 0.25)

# Start timer
start_time <- Sys.time()

# 3-month delay to surveillance scans
set.seed(3210)
v0$randomSeed<-3210

v1other$monitoringIntervalsSuspensionTime <- rep(0.25, length(v1other$monitoringIntervals))
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
Eventsandcosts(scen1.surv)
inv<-Eventsandcosts(scen1.surv)[1,2]
scr<-Eventsandcosts(scen1.surv)[2,2]
reinv<-Eventsandcosts(scen1.surv)[3,2]
nonatt<-Eventsandcosts(scen1.surv)[4,2]
monitor<-Eventsandcosts(scen1.surv)[6,2]
dropout<-Eventsandcosts(scen1.surv)[7,2]
oppdet<-Eventsandcosts(scen1.surv)[8,2]
consult<-Eventsandcosts(scen1.surv)[9,2]
elecevar<-Eventsandcosts(scen1.surv)[14,2]
elecopen<-Eventsandcosts(scen1.surv)[15,2]
rupt<-Eventsandcosts(scen1.surv)[16,2]
emerevar<-Eventsandcosts(scen1.surv)[17,2]
emeropen<-Eventsandcosts(scen1.surv)[18,2]
reintelecevar<-Eventsandcosts(scen1.surv)[21,2]
reintemerevar<-Eventsandcosts(scen1.surv)[21,2]
reintemeropen<-Eventsandcosts(scen1.surv)[23,2]
aaadead<-Eventsandcosts(scen1.surv)[24,2]
nonaaadead<-Eventsandcosts(scen1.surv)[25,2]
n<-v0$numberOfPersons
period<-0.25
thresh<-5.5
lengththresh<-0
scen1summary<-data.frame(n,period,thresh,lengththresh,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)

# Print time taken
time_one_run <- Sys.time()
diff_time <- difftime(time_one_run, start_time, units='mins')
print(paste(c("Time for one run: ", diff_time), collapse=""))

# 6-month delay to surveillance scans
v1other$monitoringIntervalsSuspensionTime <- rep(0.5, length(v1other$monitoringIntervals))
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
Eventsandcosts(scen1.surv)
inv<-Eventsandcosts(scen1.surv)[1,2]
scr<-Eventsandcosts(scen1.surv)[2,2]
reinv<-Eventsandcosts(scen1.surv)[3,2]
nonatt<-Eventsandcosts(scen1.surv)[4,2]
monitor<-Eventsandcosts(scen1.surv)[6,2]
dropout<-Eventsandcosts(scen1.surv)[7,2]
oppdet<-Eventsandcosts(scen1.surv)[8,2]
consult<-Eventsandcosts(scen1.surv)[9,2]
elecevar<-Eventsandcosts(scen1.surv)[14,2]
elecopen<-Eventsandcosts(scen1.surv)[15,2]
rupt<-Eventsandcosts(scen1.surv)[16,2]
emerevar<-Eventsandcosts(scen1.surv)[17,2]
emeropen<-Eventsandcosts(scen1.surv)[18,2]
reintelecevar<-Eventsandcosts(scen1.surv)[21,2]
reintemerevar<-Eventsandcosts(scen1.surv)[21,2]
reintemeropen<-Eventsandcosts(scen1.surv)[23,2]
aaadead<-Eventsandcosts(scen1.surv)[24,2]
nonaaadead<-Eventsandcosts(scen1.surv)[25,2]
period<-0.5
temp<-data.frame(n,period,thresh,lengththresh,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen1summary<-rbind(scen1summary,temp)


# 1-year delay to surveillance scans
v1other$monitoringIntervalsSuspensionTime <- rep(1, length(v1other$monitoringIntervals))
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
Eventsandcosts(scen1.surv)
inv<-Eventsandcosts(scen1.surv)[1,2]
scr<-Eventsandcosts(scen1.surv)[2,2]
reinv<-Eventsandcosts(scen1.surv)[3,2]
nonatt<-Eventsandcosts(scen1.surv)[4,2]
monitor<-Eventsandcosts(scen1.surv)[6,2]
dropout<-Eventsandcosts(scen1.surv)[7,2]
oppdet<-Eventsandcosts(scen1.surv)[8,2]
consult<-Eventsandcosts(scen1.surv)[9,2]
elecevar<-Eventsandcosts(scen1.surv)[14,2]
elecopen<-Eventsandcosts(scen1.surv)[15,2]
rupt<-Eventsandcosts(scen1.surv)[16,2]
emerevar<-Eventsandcosts(scen1.surv)[17,2]
emeropen<-Eventsandcosts(scen1.surv)[18,2]
reintelecevar<-Eventsandcosts(scen1.surv)[21,2]
reintemerevar<-Eventsandcosts(scen1.surv)[21,2]
reintemeropen<-Eventsandcosts(scen1.surv)[23,2]
aaadead<-Eventsandcosts(scen1.surv)[24,2]
nonaaadead<-Eventsandcosts(scen1.surv)[25,2]
period<-1
temp<-data.frame(n,period,thresh,lengththresh,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen1summary<-rbind(scen1summary,temp)

# Count the aorta sizes of people with AAA-related deaths
death_aorta_size <- get_aaa_death_aorta_sizes(scen1.surv, n=n, period=period)

# 2-year delay to surveillance scans
v1other$monitoringIntervalsSuspensionTime <- rep(2, length(v1other$monitoringIntervals))
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
Eventsandcosts(scen1.surv)
inv<-Eventsandcosts(scen1.surv)[1,2]
scr<-Eventsandcosts(scen1.surv)[2,2]
reinv<-Eventsandcosts(scen1.surv)[3,2]
nonatt<-Eventsandcosts(scen1.surv)[4,2]
monitor<-Eventsandcosts(scen1.surv)[6,2]
dropout<-Eventsandcosts(scen1.surv)[7,2]
oppdet<-Eventsandcosts(scen1.surv)[8,2]
consult<-Eventsandcosts(scen1.surv)[9,2]
elecevar<-Eventsandcosts(scen1.surv)[14,2]
elecopen<-Eventsandcosts(scen1.surv)[15,2]
rupt<-Eventsandcosts(scen1.surv)[16,2]
emerevar<-Eventsandcosts(scen1.surv)[17,2]
emeropen<-Eventsandcosts(scen1.surv)[18,2]
reintelecevar<-Eventsandcosts(scen1.surv)[21,2]
reintemerevar<-Eventsandcosts(scen1.surv)[21,2]
reintemeropen<-Eventsandcosts(scen1.surv)[23,2]
aaadead<-Eventsandcosts(scen1.surv)[24,2]
nonaaadead<-Eventsandcosts(scen1.surv)[25,2]
period<-2
temp<-data.frame(n,period,thresh,lengththresh,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen1summary<-rbind(scen1summary,temp)

# Count the aorta sizes of people with AAA-related deaths
temp <- get_aaa_death_aorta_sizes(scen1.surv, n=n, period=period)
death_aorta_size <- rbind(death_aorta_size, temp)

# 3-year delay to surveillance scans
set.seed(3210)
v0$randomSeed<-3210

v1other$monitoringIntervalsSuspensionTime <- rep(3, length(v1other$monitoringIntervals))
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
Eventsandcosts(scen1.surv)
inv<-Eventsandcosts(scen1.surv)[1,2]
scr<-Eventsandcosts(scen1.surv)[2,2]
reinv<-Eventsandcosts(scen1.surv)[3,2]
nonatt<-Eventsandcosts(scen1.surv)[4,2]
monitor<-Eventsandcosts(scen1.surv)[6,2]
dropout<-Eventsandcosts(scen1.surv)[7,2]
oppdet<-Eventsandcosts(scen1.surv)[8,2]
consult<-Eventsandcosts(scen1.surv)[9,2]
elecevar<-Eventsandcosts(scen1.surv)[14,2]
elecopen<-Eventsandcosts(scen1.surv)[15,2]
rupt<-Eventsandcosts(scen1.surv)[16,2]
emerevar<-Eventsandcosts(scen1.surv)[17,2]
emeropen<-Eventsandcosts(scen1.surv)[18,2]
reintelecevar<-Eventsandcosts(scen1.surv)[21,2]
reintemerevar<-Eventsandcosts(scen1.surv)[21,2]
reintemeropen<-Eventsandcosts(scen1.surv)[23,2]
aaadead<-Eventsandcosts(scen1.surv)[24,2]
nonaaadead<-Eventsandcosts(scen1.surv)[25,2]
n<-v0$numberOfPersons
period<-3
thresh<-5.5
lengththresh<-0
temp<-data.frame(n,period,thresh,lengththresh,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen1summary<-rbind(scen1summary,temp)


# 4-year delay to surveillance scans
v1other$monitoringIntervalsSuspensionTime <- rep(4, length(v1other$monitoringIntervals))
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
Eventsandcosts(scen1.surv)
inv<-Eventsandcosts(scen1.surv)[1,2]
scr<-Eventsandcosts(scen1.surv)[2,2]
reinv<-Eventsandcosts(scen1.surv)[3,2]
nonatt<-Eventsandcosts(scen1.surv)[4,2]
monitor<-Eventsandcosts(scen1.surv)[6,2]
dropout<-Eventsandcosts(scen1.surv)[7,2]
oppdet<-Eventsandcosts(scen1.surv)[8,2]
consult<-Eventsandcosts(scen1.surv)[9,2]
elecevar<-Eventsandcosts(scen1.surv)[14,2]
elecopen<-Eventsandcosts(scen1.surv)[15,2]
rupt<-Eventsandcosts(scen1.surv)[16,2]
emerevar<-Eventsandcosts(scen1.surv)[17,2]
emeropen<-Eventsandcosts(scen1.surv)[18,2]
reintelecevar<-Eventsandcosts(scen1.surv)[21,2]
reintemerevar<-Eventsandcosts(scen1.surv)[21,2]
reintemeropen<-Eventsandcosts(scen1.surv)[23,2]
aaadead<-Eventsandcosts(scen1.surv)[24,2]
nonaaadead<-Eventsandcosts(scen1.surv)[25,2]
period<-4
temp<-data.frame(n,period,thresh,lengththresh,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen1summary<-rbind(scen1summary,temp)


# 5-year delay to surveillance scans
v1other$monitoringIntervalsSuspensionTime <- rep(5, length(v1other$monitoringIntervals))
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
Eventsandcosts(scen1.surv)
inv<-Eventsandcosts(scen1.surv)[1,2]
scr<-Eventsandcosts(scen1.surv)[2,2]
reinv<-Eventsandcosts(scen1.surv)[3,2]
nonatt<-Eventsandcosts(scen1.surv)[4,2]
monitor<-Eventsandcosts(scen1.surv)[6,2]
dropout<-Eventsandcosts(scen1.surv)[7,2]
oppdet<-Eventsandcosts(scen1.surv)[8,2]
consult<-Eventsandcosts(scen1.surv)[9,2]
elecevar<-Eventsandcosts(scen1.surv)[14,2]
elecopen<-Eventsandcosts(scen1.surv)[15,2]
rupt<-Eventsandcosts(scen1.surv)[16,2]
emerevar<-Eventsandcosts(scen1.surv)[17,2]
emeropen<-Eventsandcosts(scen1.surv)[18,2]
reintelecevar<-Eventsandcosts(scen1.surv)[21,2]
reintemerevar<-Eventsandcosts(scen1.surv)[21,2]
reintemeropen<-Eventsandcosts(scen1.surv)[23,2]
aaadead<-Eventsandcosts(scen1.surv)[24,2]
nonaaadead<-Eventsandcosts(scen1.surv)[25,2]
period<-5
temp<-data.frame(n,period,thresh,lengththresh,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen1summary<-rbind(scen1summary,temp)

scen1summary

# Print time taken
time_all_runs <- Sys.time()
diff_time_all <- difftime(time_all_runs, start_time, units='mins')
print(paste(c("Time for all runs: ", diff_time_all), collapse=""))

######################################################################################################################################################
# SAVE RESULTS
######################################################################################################################################################

write.csv(scen1summary, "output/output_surv_scen1.csv", row.names=FALSE)
write.csv(death_aorta_size, "output/output_surv_scen1_aaadeath_aortasize.csv", row.names=FALSE)

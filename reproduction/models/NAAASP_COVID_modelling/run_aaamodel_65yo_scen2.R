## SCENARIO I2: NEW 65YO COHORT: varying attendance at initial scan
## (i) Invitation to 65 year olds delayed by 6 months
## (ii) All surveillance scans suspended by 3 months (but irrelevant here)
## (iii) Threshold for operation changed to 50cm for first 3 months (no elective ops), then revert to 55mm

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

## Persons characteristics data.frame
personData.screen <- data.frame(startAge = 65)

######################################################################################################################################################
# NEW 65-YEAR-OLD COHORT
######################################################################################################################################################

## SCENARIO I2: NEW 65YO COHORT: varying attendance at initial scan
## (i) Invitation to 65 year olds delayed by 6 months
## (ii) All surveillance scans suspended by 3 months (but irrelevant here)
## (iii) Threshold for operation changed to 50cm for first 3 months (no elective ops), then revert to 55mm

# Start timer
start_time <- Sys.time()

v1other$monitoringIntervalsSuspensionTime <- rep(0.25, length(v1other$monitoringIntervals))
v1other$aortaDiameterThresholds <- list(c(3, 4.5, 50.0), c(3, 4.5, 5.5), c(3, 4.5, 5.5))
attr(v1other$aortaDiameterThresholds, "timeCutPoints") <- c(0.25, 0.25)
v1other$inviteToScreenSuspensionTime <- 0.5

## Attendance 75%
v2$probOfAttendScreen <- setType(0.750, "probability")
set.seed(3210)
v0$randomSeed<-3210
scen1.invite <- processPersons(v0, v1other, v2, personData.screen)
scen1.invite$meanQuantities
TableOfCounts(scen1.invite, v1other)
Eventsandcosts(scen1.invite)
inv<-Eventsandcosts(scen1.invite)[1,2]
scr<-Eventsandcosts(scen1.invite)[2,2]
reinv<-Eventsandcosts(scen1.invite)[3,2]
nonatt<-Eventsandcosts(scen1.invite)[4,2]
monitor<-Eventsandcosts(scen1.invite)[6,2]
dropout<-Eventsandcosts(scen1.invite)[7,2]
oppdet<-Eventsandcosts(scen1.invite)[8,2]
consult<-Eventsandcosts(scen1.invite)[9,2]
elecevar<-Eventsandcosts(scen1.invite)[14,2]
elecopen<-Eventsandcosts(scen1.invite)[15,2]
rupt<-Eventsandcosts(scen1.invite)[16,2]
emerevar<-Eventsandcosts(scen1.invite)[17,2]
emeropen<-Eventsandcosts(scen1.invite)[18,2]
reintelecevar<-Eventsandcosts(scen1.invite)[21,2]
reintemerevar<-Eventsandcosts(scen1.invite)[21,2]
reintemeropen<-Eventsandcosts(scen1.invite)[23,2]
aaadead<-Eventsandcosts(scen1.invite)[24,2]
nonaaadead<-Eventsandcosts(scen1.invite)[25,2]
delayscr<-v1other$inviteToScreenSuspensionTime
n<-v0$numberOfPersons
attend<-v2$probOfAttendScreen
scen2summaryi<-data.frame(n,attend,delayscr,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)

# Print time taken
time_one_run <- Sys.time()
diff_time <- difftime(time_one_run, start_time, units='mins')
print(paste(c("Time for one run: ", diff_time), collapse=""))

## Attendance 65%
v2$probOfAttendScreen <- setType(0.650, "probability")
scen1.invite <- processPersons(v0, v1other, v2, personData.screen)
scen1.invite$meanQuantities
TableOfCounts(scen1.invite, v1other)
Eventsandcosts(scen1.invite)
inv<-Eventsandcosts(scen1.invite)[1,2]
scr<-Eventsandcosts(scen1.invite)[2,2]
reinv<-Eventsandcosts(scen1.invite)[3,2]
nonatt<-Eventsandcosts(scen1.invite)[4,2]
monitor<-Eventsandcosts(scen1.invite)[6,2]
dropout<-Eventsandcosts(scen1.invite)[7,2]
oppdet<-Eventsandcosts(scen1.invite)[8,2]
consult<-Eventsandcosts(scen1.invite)[9,2]
elecevar<-Eventsandcosts(scen1.invite)[14,2]
elecopen<-Eventsandcosts(scen1.invite)[15,2]
rupt<-Eventsandcosts(scen1.invite)[16,2]
emerevar<-Eventsandcosts(scen1.invite)[17,2]
emeropen<-Eventsandcosts(scen1.invite)[18,2]
reintelecevar<-Eventsandcosts(scen1.invite)[21,2]
reintemerevar<-Eventsandcosts(scen1.invite)[21,2]
reintemeropen<-Eventsandcosts(scen1.invite)[23,2]
aaadead<-Eventsandcosts(scen1.invite)[24,2]
nonaaadead<-Eventsandcosts(scen1.invite)[25,2]
delayscr<-v1other$inviteToScreenSuspensionTime
attend<-v2$probOfAttendScreen
temp<-data.frame(n,attend,delayscr,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen2summaryi<-rbind(scen2summaryi, temp)

## Attendance 55%
v2$probOfAttendScreen <- setType(0.550, "probability")
scen1.invite <- processPersons(v0, v1other, v2, personData.screen)
scen1.invite$meanQuantities
TableOfCounts(scen1.invite, v1other)
Eventsandcosts(scen1.invite)
inv<-Eventsandcosts(scen1.invite)[1,2]
scr<-Eventsandcosts(scen1.invite)[2,2]
reinv<-Eventsandcosts(scen1.invite)[3,2]
nonatt<-Eventsandcosts(scen1.invite)[4,2]
monitor<-Eventsandcosts(scen1.invite)[6,2]
dropout<-Eventsandcosts(scen1.invite)[7,2]
oppdet<-Eventsandcosts(scen1.invite)[8,2]
consult<-Eventsandcosts(scen1.invite)[9,2]
elecevar<-Eventsandcosts(scen1.invite)[14,2]
elecopen<-Eventsandcosts(scen1.invite)[15,2]
rupt<-Eventsandcosts(scen1.invite)[16,2]
emerevar<-Eventsandcosts(scen1.invite)[17,2]
emeropen<-Eventsandcosts(scen1.invite)[18,2]
reintelecevar<-Eventsandcosts(scen1.invite)[21,2]
reintemerevar<-Eventsandcosts(scen1.invite)[21,2]
reintemeropen<-Eventsandcosts(scen1.invite)[23,2]
aaadead<-Eventsandcosts(scen1.invite)[24,2]
nonaaadead<-Eventsandcosts(scen1.invite)[25,2]
delayscr<-v1other$inviteToScreenSuspensionTime
attend<-v2$probOfAttendScreen
temp<-data.frame(n,attend,delayscr,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen2summaryi<-rbind(scen2summaryi, temp)

## Attendance 45%
v2$probOfAttendScreen <- setType(0.450, "probability")
scen1.invite <- processPersons(v0, v1other, v2, personData.screen)
scen1.invite$meanQuantities
TableOfCounts(scen1.invite, v1other)
Eventsandcosts(scen1.invite)
inv<-Eventsandcosts(scen1.invite)[1,2]
scr<-Eventsandcosts(scen1.invite)[2,2]
reinv<-Eventsandcosts(scen1.invite)[3,2]
nonatt<-Eventsandcosts(scen1.invite)[4,2]
monitor<-Eventsandcosts(scen1.invite)[6,2]
dropout<-Eventsandcosts(scen1.invite)[7,2]
oppdet<-Eventsandcosts(scen1.invite)[8,2]
consult<-Eventsandcosts(scen1.invite)[9,2]
elecevar<-Eventsandcosts(scen1.invite)[14,2]
elecopen<-Eventsandcosts(scen1.invite)[15,2]
rupt<-Eventsandcosts(scen1.invite)[16,2]
emerevar<-Eventsandcosts(scen1.invite)[17,2]
emeropen<-Eventsandcosts(scen1.invite)[18,2]
reintelecevar<-Eventsandcosts(scen1.invite)[21,2]
reintemerevar<-Eventsandcosts(scen1.invite)[21,2]
reintemeropen<-Eventsandcosts(scen1.invite)[23,2]
aaadead<-Eventsandcosts(scen1.invite)[24,2]
nonaaadead<-Eventsandcosts(scen1.invite)[25,2]
delayscr<-v1other$inviteToScreenSuspensionTime
attend<-v2$probOfAttendScreen
temp<-data.frame(n,attend,delayscr,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen2summaryi<-rbind(scen2summaryi, temp)

# adding status quo (scen0) results 
v1other$monitoringIntervalsSuspensionTime <- rep(0, length(v1other$monitoringIntervals))
v1other$aortaDiameterThresholds <- list(c(3, 4.5, 5.5), c(3, 4.5, 5.5), c(3, 4.5, 5.5))
attr(v1other$aortaDiameterThresholds, "timeCutPoints") <- c(0, 0)

v2$probOfAttendScreen <- setType(0.750, "probability")
v1other$inviteToScreenSuspensionTime <- 0
scen1.invite <- processPersons(v0, v1other, v2, personData.screen)
scen1.invite$meanQuantities
TableOfCounts(scen1.invite, v1other)
Eventsandcosts(scen1.invite)
inv<-Eventsandcosts(scen1.invite)[1,2]
scr<-Eventsandcosts(scen1.invite)[2,2]
reinv<-Eventsandcosts(scen1.invite)[3,2]
nonatt<-Eventsandcosts(scen1.invite)[4,2]
monitor<-Eventsandcosts(scen1.invite)[6,2]
dropout<-Eventsandcosts(scen1.invite)[7,2]
oppdet<-Eventsandcosts(scen1.invite)[8,2]
consult<-Eventsandcosts(scen1.invite)[9,2]
elecevar<-Eventsandcosts(scen1.invite)[14,2]
elecopen<-Eventsandcosts(scen1.invite)[15,2]
rupt<-Eventsandcosts(scen1.invite)[16,2]
emerevar<-Eventsandcosts(scen1.invite)[17,2]
emeropen<-Eventsandcosts(scen1.invite)[18,2]
reintelecevar<-Eventsandcosts(scen1.invite)[21,2]
reintemerevar<-Eventsandcosts(scen1.invite)[21,2]
reintemeropen<-Eventsandcosts(scen1.invite)[23,2]
aaadead<-Eventsandcosts(scen1.invite)[24,2]
nonaaadead<-Eventsandcosts(scen1.invite)[25,2]
delayscr<-v1other$inviteToScreenSuspensionTime
attend<-0.75
temp<-data.frame(n,attend,delayscr,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen2summaryi<-rbind(scen2summaryi,temp)
scen2summaryi

# Print time taken
time_all_runs <- Sys.time()
diff_time_all <- difftime(time_all_runs, start_time, units='mins')
print(paste(c("Time for all runs: ", diff_time_all), collapse=""))

######################################################################################################################################################
# SAVE RESULTS
######################################################################################################################################################

write.csv(scen2summaryi, "output/output_65yo_scen2.csv", row.names=FALSE)

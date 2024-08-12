# One of the scenarios is provided as a test example for you to run and confirm
# whether you are getting consistent results. It has been modified to have a
# shorter model run time than as ran in the reproduction (1e6 -> 1e3, 
# parallel -> serial)

library(msm)
library(foreach)
library(iterators)
library(doParallel)

test_that("AAA model 65yo scenario 0 small version", {

  rm(list=ls())

  # Have to set working directory as the sourced script providing the input
  # parameters has a hard coded file path that assumes we are based in the
  # reproduction/directory
  setwd("../../")

  # Model
  source("functions/DES_Model.R")

  # Input parameters
  source("input/NAAASP_Men_2020-05-11/DES_Data_Input_NAAASP_Men_30years_time_horizon_2020-05-11.R")

  v1other$monitoringIntervals[4] <- 0.25 ## 3 monthly monitoring for those contraindicated
  
  ## screening group only
  v0$treatmentGroups <- "screening"

  ## List parameters
  v0
  v2
  v1distributions

  ## Set other quantities
  v0$returnEventHistories <- T ## return individual event histories
  v0$returnAllPersonsQuantities <- F ## To save memory we will not return individual HE quantitites
  v0$method <- "serial"

  v0$numberOfPersons <- 1e3
  
  ## Persons characteristics data.frame
  personData.screen <- data.frame(startAge = 65)
  
  #############################################################################
  # NEW 65-YEAR-OLD COHORT
  #############################################################################

  ## SCENARIO 0: RESULTS FOR NEw 65YO COHORT (STATUS QUO)
  set.seed(3210)
  v0$randomSeed<-3210
  scen0.invite <- processPersons(v0, v1other, v2, personData.screen)
  scen0.invite$meanQuantities

  TableOfCounts(scen0.invite, v1other)
  Eventsandcosts(scen0.invite)

  inv<-Eventsandcosts(scen0.invite)[1,2]
  scr<-Eventsandcosts(scen0.invite)[2,2]
  reinv<-Eventsandcosts(scen0.invite)[3,2]
  nonatt<-Eventsandcosts(scen0.invite)[4,2]
  monitor<-Eventsandcosts(scen0.invite)[6,2]
  dropout<-Eventsandcosts(scen0.invite)[7,2]
  oppdet<-Eventsandcosts(scen0.invite)[8,2]
  consult<-Eventsandcosts(scen0.invite)[9,2]
  elecevar<-Eventsandcosts(scen0.invite)[14,2]
  elecopen<-Eventsandcosts(scen0.invite)[15,2]
  rupt<-Eventsandcosts(scen0.invite)[16,2]
  emeropen<-Eventsandcosts(scen0.invite)[18,2]
  reintelecevar<-Eventsandcosts(scen0.invite)[21,2]
  reintemeropen<-Eventsandcosts(scen0.invite)[23,2]
  aaadead<-Eventsandcosts(scen0.invite)[24,2]
  nonaaadead<-Eventsandcosts(scen0.invite)[25,2]
  
  result <- data.frame(
    inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,
    rupt,reintelecevar,reintemeropen,aaadead,nonaaadead)
  
  # Import the expected results
  exp <- read.csv("tests/testthat/expected_results/output_65yo_scen0_1e3.csv")
  
  # Compare the dataframes
  expect_equal(result, exp)
})
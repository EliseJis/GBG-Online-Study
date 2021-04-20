##### SCRIPT FOR TIDYING CONTROL TASKS AND EXTRACTING INFO FOR MAIN DATASET #######

library(dplyr)
library(tidyr)
library(purrr)
library(rio)
library(tibble)

setwd("D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data/Merged")
path.out <- "D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data/Tidy Data/"

### CLEANING THE DATASETS
#List with participants that will be excluded
ToExclude <- list(1581739,1581744,1581751,1581758,1581761,1581763,1581766,1581771,1581784,1581788,1581790,1581801,1581803,
                  1581809,1581823,1581824,1581827,1581769,1581817,1581826,1581757,1581799,1581807,1581832,1581830,1581778,
                  2879052,2879054,2879067,2879070,2879071,2879073,1581749)

###### Stroop
Stroop_merged <- read.csv("Stroop_merged_19042021.csv")
view(Stroop_merged)

#Filter out columns and rows that won't be used (incl. excluding participants)
Stroop_tidier <- Stroop_merged %>%
  select(UTC.Timestamp, UTC.Date, Experiment.Version, Task.Name, Participant.Private.ID,Participant.Status,
         randomiser.3pls,order.2ubj, order.kx72, Trial.Number, Screen.Name,
         Reaction.Time, Response, CorrectAnswer, Correct, Timed.Out, display, Go, ISI,
         Participant.Device.Type, Participant.OS, Participant.Browser) %>%
  filter(Screen.Name=="PracticeArrow" | Screen.Name=="RealArrow",
         !(Participant.Private.ID %in% ToExclude))

#Check whether the necessary participants are exluded. Should be only FALSE
unique(Stroop_tidier$Participant.Private.ID %in% ToExclude)






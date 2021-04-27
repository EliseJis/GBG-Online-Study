##### SCRIPT FOR TIDYING CONTROL TASKS AND EXTRACTING INFO FOR MAIN DATASET #######

library(dplyr)
library(tidyr)
library(purrr)
library(rio)
library(tibble)

rm(list = ls())

date = format(Sys.Date(), "%d%m%Y")

setwd("D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data")
path.out <- "D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data/Tidy Data/"

### CLEANING THE DATASETS
#List with participants that will be excluded
ToExclude <- read.csv("ToExclude.csv")

###### Stroop##################
Stroop_merged <- read.csv("./Merged/Stroop_merged_19042021.csv")
# view(Stroop_merged)

#Filter out columns and rows that won't be used (incl. excluding participants)
Stroop_tidier <- Stroop_merged %>%
  select(UTC.Timestamp, UTC.Date, Experiment.Version, Task.Name, Participant.Public.ID,Participant.Private.ID,
         Participant.Status,randomiser.3pls,order.2ubj, order.kx72, Trial.Number, Screen.Name,
         Reaction.Time, Response, CorrectAnswer, Correct, Timed.Out, display, Go, ISI,
         Participant.Device.Type, Participant.OS, Participant.Browser) %>%
  filter(Screen.Name=="PracticeArrow" | Screen.Name=="RealArrow",
         !(Participant.Public.ID %in% ToExclude)) %>%
  unite(col="OrderControl", order.2ubj:order.kx72, sep="", remove=T) %>%
  rename("compatibility"=Go) %>%
  mutate(compatibility=recode(compatibility, CongruentLeft.png = "Stroop.Compatible",
         CongruentRight.png = "Stroop.Compatible",
         IncongruentLeft.png = "Stroop.Incompatible",
         IncongruentRight.png = "Stroop.Incompatible")) %>%
 select(-Participant.Public.ID)

#Check whether the necessary participants are exluded. Should be only FALSE
# unique(Stroop_tidier$Participant.Public.ID %in% ToExclude)
rio::export(Stroop_tidier, paste0(path.out, "Stroop_tidier_",date, ".csv"))

### SCORES STROOP
Stroop_scores <- Stroop_tidier %>%
  filter(Correct==1 | Screen.Name=="RealArrow") %>%
  group_by(Participant.Private.ID, compatibility) %>%
  summarise(RT =round(mean(Reaction.Time),3)) %>%
  mutate(Stroop.SRC = round(RT[compatibility=="Stroop.Incompatible"]-RT[compatibility=="Stroop.Compatible"]),3) %>%
  ungroup() %>%
  spread(compatibility, RT)

rio::export(Stroop_scores, paste0(path.out, "Stroop_scores_",date, ".csv"))



###### Deary Liewald - SoP #######################
SoP_merged <- read.csv("./Merged/SoP_merged_19042021.csv")
# view(SoP_merged)
colnames(SoP_merged)

#Filter out columns and rows that won't be used (incl. excluding participants)
SoP_tidier <- SoP_merged %>%
  select(UTC.Timestamp, UTC.Date, Experiment.Version, Task.Name, Participant.Private.ID, 
         Participant.Public.ID,Participant.Status,
         randomiser.3pls,order.2ubj, order.kx72, Trial.Number, Screen.Name,
         Reaction.Time,  Correct, Response, Answer1, display, ISI,
         Participant.Device.Type, Participant.OS, Participant.Browser) %>%
  filter(display=="SRT_practicetrials" | display=="SRT_realtrials" | display=="CRT_practicetrials" | display=="CRT_realtrials",
         !(Participant.Public.ID %in% ToExclude)) %>%
  unite(col="OrderControl", order.2ubj:order.kx72, sep="", remove=T) %>%
  rename("SoPTask"=Screen.Name) %>%
  mutate(SoPTask = recode(SoPTask, SRT_PracticeTrials = "SRT_practice",
                          SRT_RealTrials="SRT_real",
                          CRT_PracticeTrials="CRT_practice",
                          CRT_RealTrials="CRT_real"),
         Reaction.Time= as.numeric(Reaction.Time)) %>%
 select(-Participant.Public.ID)

#Check whether the necessary participants are exluded. Should be only FALSE
# unique(SoP_tidier$Participant.Public.ID %in% ToExclude)
rio::export(SoP_tidier, paste0(path.out, "SoP_tidier_",date, ".csv"))

### SCORES SoP
SoP_scores <- SoP_tidier %>%
  filter(Correct==1) %>%
  filter(SoPTask=="SRT_real" | SoPTask=="CRT_real") %>%
  group_by(Participant.Private.ID, SoPTask) %>%
  summarise(RT = mean(Reaction.Time)) %>%
  ungroup() %>%
  spread(SoPTask, RT) %>%
  rename("SoP.CRT"=CRT_real, "SoP.SRT"=SRT_real)

rio::export(SoP_scores, paste0(path.out, "SoP_scores_",date, ".csv"))


###### Digit Reorder - WM #######################
WM_merged <- read.csv("./Merged/WM_merged_19042021.csv")
# view(WM_merged)
colnames(WM_merged)

#Filter out columns and rows that won't be used (incl. excluding participants)
WM_tidier <- WM_merged %>%
  select(UTC.Timestamp, UTC.Date, Experiment.Version, Task.Name, Participant.Private.ID,
         Participant.Public.ID,Participant.Status,
         randomiser.3pls,order.2ubj, order.kx72, TimeFix, TimeWhite,
         Response, CorrectAns,  Correct, Reaction.Time, display, Attempt,
         Participant.Device.Type, Participant.OS, Participant.Browser) %>%
  filter(is.na(Attempt)==FALSE,
         !(Participant.Public.ID %in% ToExclude)) %>%
  unite(col="OrderControl", order.2ubj:order.kx72, sep="", remove=T) %>%
  select(-Attempt, -Participant.Public.ID)

#Check whether the necessary participants are exluded. Should be only FALSE
# unique(WM_tidier$Participant.Public.ID %in% ToExclude)
rio::export(WM_tidier, paste0(path.out, "WM_tidier_",date, ".csv"))

### SCORES WM
WM_scores <- WM_tidier %>%
  filter(display=="RealTest") %>%
  mutate(Ndigits= str_count(CorrectAns)) %>%
  group_by(Participant.Private.ID) %>%
  summarise(WM.Score = sum(Ndigits*Correct)) %>%
  ungroup()

rio::export(WM_scores, paste0(path.out, "WM_scores_",date, ".csv"))



###Load packages
library(tidyverse)
library(dplyr)
library(purrr)
#library(rJava)
library(rio)
library(ggplot2)
library(psych)

#Clean Environment
rm(list=ls())

#Obtain date
date = format(Sys.Date(), "%d%m%Y")

#setting working directory
setwd("D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data")
path.out <- "D:/OneDrive - Lancaster University/PhD project/ParticipantTesting/Online Testing/Data/Raw data/All_data/Tidy Data/"

#Read in data
PNobjects <- as_tibble(read.csv("./Merged/PNobj_merged_19042021.csv"))
PNactions <- as_tibble(read.csv("./Merged/PNact_merged_19042021.csv"))
stroop <- read.csv("./Tidy Data/Stroop_scores_27042021.csv")
SoP <- read.csv("./Tidy Data/SoP_scores_27042021.csv")
WM <- read.csv("./Tidy Data/WM_scores_27042021.csv")
ToExclude <- read.csv("ToExclude.csv") #Excluded participants

### OBJECT NAMING ####
PNobjects_tidier <- PNobjects %>%
  #Filter out double rows and first stimulus
  filter((display=="PracticeTrials" | display == "RealTrials") & 
           (Zone.Name=="content" & picname!="lionExclude")) %>%
  #Filter columns of interest
  select(UTC.Date, Experiment.Version, Participant.Public.ID, Participant.Private.ID, Participant.Status,
         Task.Name,randomiser.3pls, order.2ubj, order.kx72, randomiser.2p1e, randomiser.4bdx,
         Trial.Number, Reaction.Time, picname, type, picnum, frequency, freq_HAL_ELP, AoA, AoA_ELP, picfric, 
         NameAgreement, PNameAgreement, Nsyll, Nchar, Nphon, VisComplex, ShareName, WordComplexity,
         ConceptualComplexity, OrthNeighb, OrthNeighb_ELP, PhonNeighb, PhonNeighb_ELP, SemNeigD,
         Participant.Device, Participant.OS, Participant.Browser, Participant.Monitor.Size, Participant.Viewport.Size) %>%
  #Merge columns
  unite(col="OrderControl", order.2ubj:order.kx72, sep="", remove=T) %>%
  unite(col="List", randomiser.2p1e:randomiser.4bdx, sep="", remove=T) %>%
  #Remove uncomplete participants and excluded participants
  filter(Participant.Status=="complete",
         !(Participant.Public.ID %in% ToExclude)) %>%
  #Change Columnnames
  rename("OrderLang"=randomiser.3pls, "durStim"=Reaction.Time, "ID"=Participant.Private.ID) %>%
  select(-Participant.Status)

  #Check whether the necessary participants are exluded. Should be only FALSE (check before filtering out Public.ID)
  # unique(PNobjects_tidier$Participant.Public.ID %in% ToExclude)

#Save tidier file
rio::export(PNobjects_tidier, paste0(path.out, date, "_PNobjects_tidier.csv"))

  
#ADD CONTROL MEASURES
PNobjects_tidiest <- PNobjects_tidier %>%
  left_join(stroop, by=c("ID" = "Participant.Private.ID")) %>%
  left_join(SoP, by=c("ID"="Participant.Private.ID")) %>%
  left_join(WM, by=c("ID"="Participant.Private.ID"))

rio::export(PNobjects_tidiest, paste0(path.out, date, "_PNobjects_tidiest.csv"))

### ACTION NAMING ####
PNactions_tidier <- PNactions %>%
  #Filter out double rows and first stimulus
  filter((display=="PracticeTrials" | display == "RealTrials") & 
           (Zone.Name=="content" & picname!="barkExclude")) %>%
  #Filter columns of interest
  select(UTC.Date, Experiment.Version, Participant.Public.ID, Participant.Private.ID, Participant.Status,
         Task.Name,randomiser.3pls, order.2ubj, order.kx72, randomiser.csgp, randomiser.z3o5,
         Trial.Number, Reaction.Time, picname, type, picnum, frequency, freq_HAL_ELP, AoA, AoA_ELP, picfric, 
         NameAgreement, PNameAgreement, Nsyll, Nchar, Nphon, VisComplex, ShareName, WordComplexity,
         ConceptualComplexity, OrthNeighb, OrthNeighb_ELP, PhonNeighb, PhonNeighb_ELP, SemNeigD,
         Participant.Device, Participant.OS, Participant.Browser, Participant.Monitor.Size, Participant.Viewport.Size) %>%
  #Merge columns
  unite(col="OrderControl", order.2ubj:order.kx72, sep="", remove=T) %>%
  unite(col="List", randomiser.csgp:randomiser.z3o5, sep="", remove=T) %>%
  #Remove uncomplete participants and excluded participants
  filter(Participant.Status=="complete",
         !(Participant.Public.ID %in% ToExclude)) %>%
  #Change Columnnames
  rename("OrderLang"=randomiser.3pls, "durStim"=Reaction.Time, "ID"=Participant.Private.ID) %>%
  select(-Participant.Status)

#Check whether the necessary participants are exluded. Should be only FALSE (check before filtering out Public.ID)
# unique(PNobjects_tidier$Participant.Public.ID %in% ToExclude)

#Save tidier file
rio::export(PNactions_tidier, paste0(path.out, date, "_PNactions_tidier.csv"))

#ADD CONTROL MEASURES
PNactions_tidiest <- PNactions_tidier %>%
  left_join(stroop, by=c("ID" = "Participant.Private.ID")) %>%
  left_join(SoP, by=c("ID"="Participant.Private.ID")) %>%
  left_join(WM, by=c("ID"="Participant.Private.ID"))

rio::export(PNactions_tidiest, paste0(path.out, date, "_PNactions_tidiest.csv"))

